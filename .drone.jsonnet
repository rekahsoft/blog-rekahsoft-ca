// See: https://github.com/kradalby/drone-jsonnet/blob/master/drone.libsonnet

local droneStatus = ['success', 'failure'];

local pipeline = {
  new():: self.withKind("pipeline"),
  withName(name):: self + { name: name },
  withKind(kind):: self + { kind: kind },
  withType(type):: self + { type: type },
  withNode(node):: self + { node: node },
  withTrigger(trigger):: self + { trigger: trigger },
  withDependsOn(n):: self + { depends_on: n },
  withNodeSelector(ns):: self + { node_selector: ns },
  withSteps(steps):: self + if std.type(steps) == 'array' then { steps: steps } else { steps: [steps] },
  step:: {
    new(name='', image=''):: self.withName(name).withImage(image).withPullIfNotExists(),
    withName(name):: self + { name: name },
    withImage(image):: self + if image != '' then { image: image } else {},
    withAlwaysPull():: self + { pull: 'always' },
    withPullIfNotExists():: self + { pull: 'if-not-exists' },
    withCommands(commands):: self + if std.type(commands) == 'array' then { commands: commands } else { commands: [commands] },
    withTrigger(trigger):: self + { trigger: trigger }, // TODO: this is duplicated in pipeline object
    withEnv(envs):: self + { environment: envs },
    withWhen(when):: self + { when: when },
    withSettings(settings):: self + { settings: settings },
  },
  when:: {
    new():: self + {},
    withBranch(branch):: self + if std.type(branch) == 'array' then { branch: branch } else { branch: [branch] },
    withEvent(e):: self + if std.type(e) == 'array' then { event: e } else { event: [e] },
    withStatus(s):: self + if std.type(s) == 'array' then { status: s } else { status: [s] },
    withStatusAll():: self.withStatus(droneStatus),
  },
};

local trigger = {
  new():: self + {},
  withBranch(branch):: self + if std.type(branch) == 'array' then { branch: branch } else { branch: [branch] },
  withEvent(e):: self + if std.type(e) == 'array' then { event: e } else { event: [e] },
  withStatus(s):: self + if std.type(s) == 'array' then { status: s } else { status: [s] },
  withStatusAll():: self.withStatus(droneStatus),
};


local env_from_secret(dict) = {
  [key]: {
    from_secret: dict[key],
  }
  for key in std.objectFields(dict)
};

local guix_pipeline(name) = pipeline.new()
  .withName(name)
  .withType("docker")
  .withNode({ "guix": "on"});

local guix_step(name,
  commands,
  image="docker.nexus.home.rekahsoft.ca/guix:latest") =
    pipeline.step.new(name, image).withPullIfNotExists().withCommands(commands);

local guix_step_time_machine(name,
  commands,
  cwd=".",
  channels="channels.scm",
  image="docker.nexus.home.rekahsoft.ca/guix:latest") =
    pipeline.step.new(name, image).withPullIfNotExists().withCommands(
      (if cwd == "."
      then [] else [std.format("cd %s", cwd)]) +
      std.map(function(i) std.format("guix time-machine -C %s -- %s", [channels, i]),
        if std.type(commands) == 'array' then commands else [commands]));

local dronePromoteCmd(env) = [
  "export DRONE_SERVER=\"${DRONE_SYSTEM_PROTO}://${DRONE_SYSTEM_HOST}\"",
  std.format('DRONE_PROMOTED_PIPELINE_ID=$(drone build promote --format \'{{ .Number }}\' "$DRONE_REPO" "$DRONE_BUILD_NUMBER" "%s")', env),
  'while status="$(drone build info $DRONE_REPO $DRONE_PROMOTED_PIPELINE_ID)"; do
case "$status" in
  running)
    sleep 30s
    ;;
  success)
    break
    ;;
  failure|error|killed)
    echo "Promoted job with id $DRONE_PROMOTED_PIPELINE_ID failed with status \'$status\'."
    exit 1
    ;;
  *)
    echo "Unknown pipeline status \'$status\'."
    exit 1
esac
done',
];

local promoteStep(env,
  secret_name_drone_token="drone_token",
  image="docker.nexus.home.rekahsoft.ca/drone/cli:1.4-alpine") = pipeline.step.new(std.format("promote-%s", env), image)
    .withWhen(pipeline.when.new()
      .withBranch("master")
      .withEvent("push"))
    .withCommands(dronePromoteCmd(env))
    .withEnv(env_from_secret({
      DRONE_TOKEN: "drone_token"
    }));


local deployStep(name, target=name, args=[]) = guix_step_time_machine(
  name,
  std.format('shell -CN -E "^AWS.*$" -- make %s ENV="${DRONE_DEPLOY_TO}" %s', [target, std.join(" ", args)]),
  cwd="infra",
  channels="../channels.scm")
    .withEnv({ PLAN: "out.plan" } + env_from_secret({
      AWS_ACCESS_KEY_ID: "aws_access_key_id",
      AWS_SECRET_ACCESS_KEY: "aws_secret_access_key",
    }));


[
  guix_pipeline("validate").withTrigger(trigger.new().withEvent(["push", "pull_request", "tag"])).withSteps([
    guix_step_time_machine("build", "build -f guix.scm"),
    promoteStep("staging"),
    promoteStep("production"),
  ]),

  guix_pipeline("deploy").withTrigger(trigger.new().withEvent("promote")).withSteps([
    deployStep("init", "setup"),
    deployStep("plan"),
    deployStep("deploy"),
  ])
]
