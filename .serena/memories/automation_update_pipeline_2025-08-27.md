Added automated Serena snapshot updates:
- PowerShell script: scripts/update_serena_snapshot.ps1 writes project_tree_{latest,date}.md, project_inventory_{latest,date}.md, project_overview_{latest,date}.md into .serena/memories.
- CMake: POST_BUILD on target Clear invokes the script on Windows/PowerShell.
- VS Code task: .vscode/tasks.json with label "Update Serena snapshots" to run the script manually.
Validated by executing the script once successfully on 2025-08-27.