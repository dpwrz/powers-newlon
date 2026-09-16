# Existing Cloudflare website repo + separate private R model repo

This is the easiest path if your current Cloudflare deployment already comes from a web-only GitHub repository and you do not want to restructure it.

## Repositories
- private model repo: e.g. `fantasy-model-engine`
- existing Cloudflare repo: e.g. `fantasy-model-web`

The supplied `fantasy-live-refresh.yml` supports this layout automatically when two repository secrets are present.

## One-time setup

1. Push the R project + Model 2.5 runtime files + this live patch to the private model repository.
2. Create a **fine-grained GitHub personal access token** restricted to the website repository with `Contents: Read and write`.
3. In the model repository: Settings -> Secrets and variables -> Actions -> Repository secrets.
4. Add:
   - `WEB_REPO` = `YOUR_GITHUB_USERNAME/fantasy-model-web`
   - `WEB_REPO_TOKEN` = the fine-grained token
5. Leave Cloudflare connected to the existing web repository. No Cloudflare change is needed.
6. Run Actions -> **Fantasy Model live refresh** -> Run workflow -> Force = true.

After each successful refresh the model job writes `web/public/data/model_snapshot.json` (with an output fallback for compatibility), then publishes that file into the web repository at `public/data/model_snapshot.json`. The push to the existing web repo triggers the normal Cloudflare deployment.

If those two secrets are absent, the cross-repository steps are skipped and the same workflow behaves as a single-repository workflow.
