library(shiny)
library(httr)
library(jsonlite)
library(glue)
library(dplyr)

# ---- USER SETTINGS ----
github_user <- "emcdo411"
repo <- "mssa3demo"
local_repo_path <- "C:\\Users\\Veteran\\Documents\\GitHub\\dockrintel-ai-logs"
cmd_demo_path <- "C:\\mssa3demo"

# ---- AUTHENTICATION ----
gh_token <- Sys.getenv("GITHUB_PAT")
gh_header <- add_headers(
  Authorization = paste("Bearer", gh_token),
  Accept = "application/vnd.github+json"
)

# ---- API HELPER FUNCTION ----
gh_get <- function(endpoint) {
  url <- glue("https://api.github.com/repos/{github_user}/{repo}/{endpoint}")
  res <- GET(url, gh_header)
  if (status_code(res) == 200) {
    fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
  } else {
    warning(glue("GitHub API error: {status_code(res)} on {endpoint}"))
    NULL
  }
}

# ---- SHINY UI ----
ui <- fluidPage(
  titlePanel("🧠 RepoCoach: GitHub Command Assistant"),
  textInput("nl_input", "Ask RepoCoach (e.g., 'merge last PR', 'how do I open my repo?')", ""),
  verbatimTextOutput("report"),
  verbatimTextOutput("suggestion")
)

# ---- SHINY SERVER ----
server <- function(input, output) {
  output$report <- renderPrint({
    branches <- gh_get("branches")
    branch_names <- if (!is.null(branches)) branches$name else "No branches found"
    
    prs <- gh_get("pulls")
    pr_count <- if (!is.null(prs) && is.data.frame(prs)) nrow(prs) else 0
    pr_titles <- if (pr_count > 0) paste(prs$title, collapse = "\n    ") else "None"
    
    repo_info <- GET(glue("https://api.github.com/repos/{github_user}/{repo}"), gh_header)
    repo_data <- fromJSON(content(repo_info, "text", encoding = "UTF-8"))
    
    cat(glue("
📊 RepoCoach Report for '{repo}'

✔️ Branches found: {length(branch_names)}
    {paste(branch_names, collapse = ', ')}

📬 Open Pull Requests: {pr_count}
    {pr_titles}

📅 Last updated: {repo_data$updated_at}
⭐ Stars: {repo_data$stargazers_count} | 🍴 Forks: {repo_data$forks_count}

🧠 AI Summary:
- The repo was last active on {repo_data$updated_at}.
- You have {pr_count} open pull request(s).
- Top branch is likely 'main' or 'master'. Consider cleaning up stale branches if needed.

💡 Suggested GitHub Commands:
- Clone: git clone https://github.com/{github_user}/{repo}.git
- New branch: git checkout -b feature/my-new-branch
- View PRs: gh pr list
- Merge PR: gh pr merge <PR-number> --merge
- Create PR: gh pr create --base main --head feature/my-new-branch --title 'Add feature'
- Delete branch: git branch -d old-branch-name
- Remote delete: git push origin --delete old-branch-name
- View issues: gh issue list
- New issue: gh issue create --title 'Bug: ...' --body 'Steps to reproduce...'
"))
  })
  
  output$suggestion <- renderPrint({
    user_input <- tolower(input$nl_input)
    if (user_input == "") return("🧠 Waiting for your command...")
    
    cat("🧠 Interpreted Suggestion:\n")
    
    if (grepl("merge", user_input)) {
      cat("→ gh pr merge <last-PR-number> --merge\n")
    } else if (grepl("create.*branch", user_input)) {
      cat("→ First, make sure you're inside a Git repository.\n")
      cat("   If you're starting a new project:\n")
      cat("     git init\n")
      cat("   If you're cloning from GitHub:\n")
      cat(glue("     git clone https://github.com/{github_user}/{repo}.git\n"))
      cat(glue("     cd {repo}\n"))
      cat("→ Then create a new branch:\n")
      cat("     git checkout -b feature/my-new-branch\n")
    } else if (grepl("delete.*branch", user_input)) {
      cat("→ git branch -d old-branch-name\n")
    } else if (grepl("switch|checkout", user_input)) {
      cat("→ git checkout main\n")
    } else if (grepl("new repo|create.*repo", user_input)) {
      cat("→ gh repo create my-new-repo --public\n")
    } else if (grepl("list.*pr|show.*pull", user_input)) {
      cat("→ gh pr list\n")
    } else if (grepl("clone", user_input)) {
      cat(glue("→ git clone https://github.com/{github_user}/{repo}.git\n"))
    } else if (grepl("open.*folder|cd|change.*directory", user_input)) {
      cat(glue("
📁 CMD Instructions:
  C:\\Users\\Veteran>md {cmd_demo_path}
  C:\\Users\\Veteran>cd {cmd_demo_path}

📁 PowerShell:
  New-Item -ItemType Directory -Path '{cmd_demo_path}'
  Set-Location '{cmd_demo_path}'

🖥️ VS Code:
  code {cmd_demo_path}

📦 GitHub Desktop Install Path:
  C:\\Users\\Veteran\\AppData\\Local\\GitHubDesktop\\bin\\github
"))
    } else {
      cat("→ 🤔 Sorry, I couldn't interpret that command (yet).")
    }
  })
}

# ---- LAUNCH APP ----
shinyApp(ui, server)
