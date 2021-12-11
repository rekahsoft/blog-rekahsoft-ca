local ci = import '.drone/ci.libsonnet';

local guix_pipeline(name) = ci.pipeline.new()
  .withName(name)
  .withType("docker")
  .withNode({ "guix": "on"});

local guix_step(name,
  commands,
  image="docker.nexus.home.rekahsoft.ca/guix:latest") =
    ci.pipeline.step.new(name, image).withPullIfNotExists().withCommands(commands);

local guix_step_time_machine(name,
  commands,
  cwd=".",
  channels="channels.scm",
  image="docker.nexus.home.rekahsoft.ca/guix:latest") =
    ci.pipeline.step.new(name, image).withPullIfNotExists().withCommands(
      // Conditionally change directory
      (if cwd == "."
      then [] else [std.format("cd %s", cwd)]) +
      // Expand provide guix commands into executable shell
      std.map(function(i) std.format("guix time-machine -C %s -- %s", [channels, i]),
        if std.type(commands) == 'array' then commands else [commands]));

local deployStep(name, target=name, args=[]) = guix_step_time_machine(
  name,
  std.format('shell -m manifest.scm -- make %s ENV="${DRONE_DEPLOY_TO}" %s', [target, std.join(" ", args)]),
  cwd="infra",
  channels="../channels.scm")
    .withEnv({ PLAN: "out.plan" } + ci.env_from_secret({
      AWS_ACCESS_KEY_ID: "aws_access_key_id",
      AWS_SECRET_ACCESS_KEY: "aws_secret_access_key",
    }));

[
  guix_pipeline("validate").withTrigger(ci.trigger.new().withEvent(["push", "pull_request", "tag"])).withSteps([
    guix_step_time_machine("build", "build -f guix.scm"),
    ci.promoteStep("staging"),
    ci.promoteStep("production"),
  ]),

  guix_pipeline("deploy").withTrigger(ci.trigger.new().withEvent("promote")).withSteps([
    deployStep("init", "setup"),
    deployStep("plan").withRuntimeEnvVar({
      TF_VAR_site_static_files_dir: "$(guix time-machine -C channels.scm -- build -f guix.scm | grep -e '^.*-site$')"
    }),
    deployStep("deploy"),
  ])
]
