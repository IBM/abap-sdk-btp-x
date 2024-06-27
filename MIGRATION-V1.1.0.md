# Upgrading to ABAP SDK version 1.1.0

<details>
  <summary>Table of Contents</summary>

- [Breaking changes](#breaking-changes)
  - [Breaking changes by service](#breaking-changes-by-service)
    - [watsonx.ai V1](#watsonxai-v1)
  - [New features by service](#new-features-by-service)
    - [watsonx.ai V1](#watsonxai-v1-1)

</details>

## Breaking changes

### Breaking changes by service

#### watsonx.ai V1

- Method `Deployments_Text_Generation`

  - Parameter `i_DeploymentTextGenRequest` of type `t_Deployment_Text_Gen_Request` was replaced with parameter `i_Body` of type `t_Deployment_Text_Gen`.

### New features by service

#### watsonx.ai V1

 - New method `Text_Embeddings`
