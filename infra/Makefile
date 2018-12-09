# Set default target for make > 3.80
.DEFAULT_GOAL := default

# Default make to the deploy target
.PHONY: default
default: deploy

SELECTED_WORKSPACE := $(shell terraform workspace show)
ENV := $(if $(ENV),$(ENV),$(SELECTED_WORKSPACE))

.PHONY: setup
setup:
ifneq ($(SELECTED_WORKSPACE),$(ENV))
ifndef CI
	@terraform workspace select $(ENV)
endif
endif

.PHONY: init
init:
	@terraform init

.PHONY: plan
plan: setup
	@terraform plan \
		$(if $(PLAN),--out $(PLAN)) \
		$(if $(ENV),--var-file=$(ENV).tfvars) \
		$(ARGS)

.PHONY: deploy
deploy: setup
	@terraform apply \
		$(if $(PLAN),$(PLAN)) \
		$(ARGS)

.PHONY: destroy
destroy: setup
	@terraform destroy \
		$(if $(ENV),--var-file=$(ENV).tfvars) \
		$(ARGS)
