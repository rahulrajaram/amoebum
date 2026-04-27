(in-package :amoebum)

(deftool git-status ()
  "Return repository status: branch, tracking, staged, and unstaged changes."
  (:permission :auto)
  (:dangerous nil)
  (:category :git)
  (:timeout 30)
  (%git-status-data))

(deftool git-diff-branch ((base-branch (or null string)
                             :description "Optional base branch override (defaults to main/master detection)."
                             :default nil))
  "Return current-branch diff vs base branch for PR context."
  (:permission :auto)
  (:dangerous nil)
  (:category :git)
  (:timeout 60)
  (%git-diff-branch-data :base-branch base-branch))

(deftool git-commit ((files (or null list string)
                    :description "Optional explicit files to stage before commit."
                    :default nil)
                     (co-author (or null string)
                      :description "Co-Authored-By identity to append."
                      :default nil)
                     (model (or null string)
                      :description "Optional model override for commit message generation."
                      :default nil)
                     (amend boolean
                      :description "Request amend mode (requires ALLOW-AMEND true)."
                      :default nil)
                     (allow-amend boolean
                      :description "Explicit acknowledgement to permit amend."
                      :default nil))
  "Stage explicit files, generate commit message from staged diff, and create commit."
  (:permission :full-auto)
  (:dangerous t)
  (:category :git)
  (:timeout 180)
  (%git-commit-tool-data :files files
                         :co-author co-author
                         :model model
                         :amend amend
                         :allow-amend allow-amend))

(deftool create-pr ((base-branch (or null string)
                        :description "Optional base branch override (defaults to main/master detection)."
                        :default nil)
                    (model (or null string)
                      :description "Optional model override for PR title/body generation."
                      :default nil))
  "Generate pull request title/body from full branch history, push if needed, and create PR via gh."
  (:permission :full-auto)
  (:dangerous t)
  (:category :git)
  (:timeout 240)
  (%git-create-pr-tool-data :base-branch base-branch
                            :model model))
