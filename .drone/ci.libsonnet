{
  local ci = self,
  local droneStatus = ['success', 'failure'],

  pipeline:: {
    new()::
      self.withKind("pipeline"),

    withName(name)::
      self + { name: name },

    withKind(kind)::
      self + { kind: kind },

    withType(type)::
      self + { type: type },

    withNode(node)::
      self + { node: node },

    withTrigger(trigger)::
      self + { trigger: trigger },

    withDependsOn(n)::
      self + { depends_on: n },

    withNodeSelector(ns)::
      self + { node_selector: ns },

    withSteps(steps)::
      self + if std.type(steps) == 'array'
             then { steps: steps }
             else { steps: [steps] },

    step:: {
      new(name='', image='')::
        self.withName(name).withImage(image).withPullIfNotExists(),

      withName(name)::
        self + { name: name },

      withImage(image)::
        self + if image != '' then { image: image } else {},

      withAlwaysPull()::
        self + { pull: 'always' },

      withPullIfNotExists()::
        self + { pull: 'if-not-exists' },

      withCommands(commands)::
        self + if std.type(commands) == 'array'
               then { commands: commands }
               else { commands: [commands] },

      withTrigger(trigger)::
        self + { trigger: trigger }, // TODO: this is duplicated in pipeline object

      withEnv(envs)::
        self + { environment+: envs },

      withRuntimeEnvVar(envs)::
        local existingCmds = if std.objectHas(self, "commands") then self.commands else [];
        self + {
          commands: std.map(function (i) std.format('export %s="%s"', [i, envs[i]]),
                            std.objectFields(envs)) + existingCmds
        },

      withWhen(when)::
        self + { when: when },

      withSettings(settings)::
        self + { settings: settings },
    },

    when:: {
      new()::
        self + {},

      withBranch(branch)::
        self + if std.type(branch) == 'array'
               then { branch: branch }
               else { branch: [branch] },

      withEvent(e)::
        self + if std.type(e) == 'array'
               then { event: e }
               else { event: [e] },

      withStatus(s)::
        self + if std.type(s) == 'array'
               then { status: s }
               else { status: [s] },

      withStatusAll()::
        self.withStatus(droneStatus),
    },
  },

  trigger:: {
    new()::
      self + {},

    withBranch(branch)::
      self + if std.type(branch) == 'array'
             then { branch: branch }
             else { branch: [branch] },

    withEvent(e)::
      self + if std.type(e) == 'array'
             then { event: e }
             else { event: [e] },

    withStatus(s)::
      self + if std.type(s) == 'array'
             then { status: s }
             else { status: [s] },

    withStatusAll()::
      self.withStatus(droneStatus),
  },

  env_from_secret(dict):: {
    [key]: {
      from_secret: dict[key],
    }
    for key in std.objectFields(dict)
  },

  guix:: {
    pipeline(name)::
      ci.pipeline.new()
      .withName(name)
      .withType("docker")
      .withNode({ "guix": "on"}),

    step(name, commands, image="docker.nexus.home.rekahsoft.ca/guix-builder:latest")::
      ci.pipeline.step.new(name, image).withPullIfNotExists().withCommands(commands),

    stepTimeMachine(name, commands, cwd=".", channels="channels.scm", image="docker.nexus.home.rekahsoft.ca/guix-builder:latest")::
      ci.pipeline.step.new(name, image).withPullIfNotExists().withCommands(
        // Conditionally change directory
        (if cwd == "."
         then [] else [std.format("cd %s", cwd)]) +
        // Expand provide guix commands into executable shell
        std.map(function(i) std.format("guix time-machine -C %s -- %s", [channels, i]),
                if std.type(commands) == 'array' then commands else [commands])),
  },

  promoteStep(env,
              secret_name_drone_token="drone_token",
              image="docker.nexus.home.rekahsoft.ca/drone/cli:1.4-alpine")::
    local dronePromoteCmd(env) = [
      "export DRONE_SERVER=\"${DRONE_SYSTEM_PROTO}://${DRONE_SYSTEM_HOST}\"",
      "export DRONE_TOKEN",
      std.format('DRONE_PROMOTED_PIPELINE_ID=$(drone build promote --format \'{{ .Number }}\' "$DRONE_REPO" "$DRONE_BUILD_NUMBER" "%s")', env),
      'while status="$(drone build info --format \'{{ .Status }}\' $DRONE_REPO $DRONE_PROMOTED_PIPELINE_ID)"; do
case "$status" in
  pending|running)
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

   ci.pipeline.step.new(std.format("promote-%s", env), image)
    .withWhen(ci.pipeline.when.new()
              .withBranch("master")
              .withEvent("push"))
    .withCommands(dronePromoteCmd(env))
    .withEnv(ci.env_from_secret({
      DRONE_TOKEN: "drone_token"
    })),

  awsDeployStep(name, target=name, args=[])::
    ci.guix.stepTimeMachine(
      name,
      std.format('shell -m manifest.scm -- make %s ENV="${DRONE_DEPLOY_TO}" %s', [target, std.join(" ", args)]),
      cwd="infra",
      channels="../channels.scm")
    .withEnv(ci.env_from_secret({
      AWS_ACCESS_KEY_ID: "aws_access_key_id",
      AWS_SECRET_ACCESS_KEY: "aws_secret_access_key",
    })),
}
