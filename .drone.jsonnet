local ci = import '.drone/ci.libsonnet';

[
  ci.guix.pipeline("validate").withTrigger(ci.trigger.new().withEvent(["push", "pull_request", "tag"])).withSteps([
    ci.guix.stepTimeMachine("build", "build -f guix.scm"),
    ci.promoteStep("staging"),
    ci.promoteStep("production"),
  ]),

  ci.guix.pipeline("deploy").withTrigger(ci.trigger.new().withEvent("promote")).withSteps([
    ci.awsDeployStep("init", "setup"),
    ci.awsDeployStep("plan").withEnv({
      PLAN: "out.plan"
    }).withRuntimeEnvVar({
      TF_VAR_site_static_files_dir: "$(guix time-machine -C channels.scm -- build -f guix.scm | grep -e '^.*-site$')"
    }),
    ci.awsDeployStep("deploy").withEnv({
      PLAN: "out.plan"
    }),
  ])
]
