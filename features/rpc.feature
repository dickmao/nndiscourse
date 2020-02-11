@refresh_token
Scenario: start gnus
  Given gnus start
  And I dump buffer

@extant
Scenario: gnus-demon-scan-news while summary buffer open, then auto-rescore upon quitting summary buffer
  Given of-record unreads for "nndiscourse:emacs" is 94
  When I go to word "nndiscourse:emacs"
  And I press "RET"
  Then I should be in buffer "*Summary nndiscourse:emacs*"
  And I press "C-k"
  Then prospective unreads for "nndiscourse:emacs" is 93
  And I scan news
  And I switch to buffer "*Summary nndiscourse:emacs*"
  Then of-record unreads for "nndiscourse:emacs" is 187
  And prospective unreads for "nndiscourse:emacs" is 93
  And I press "q"
  Then I should be in buffer "*Group*"
  Then of-record unreads for "nndiscourse:emacs" is 186
  And I scan news
  Then of-record unreads for "nndiscourse:emacs" is 280
  When I go to word "nndiscourse:emacs"
  And I press "RET"
  Then I should be in buffer "*Summary nndiscourse:emacs*"
  And I go to word "DoreenMich"
  And I press "C-k"
  And prospective unreads for "nndiscourse:emacs" is 273
  And I scan news
  When I go to word "nndiscourse:emacs"
  And I press "RET"
  Then prospective unreads for "nndiscourse:emacs" is 273
  And of-record unreads for "nndiscourse:emacs" is 312
  And I press "q"
  Then of-record unreads for "nndiscourse:emacs" is 303

@reply_nologin
Scenario: reply not having logged in yet
  When I go to word "nndiscourse:emacs"
  And I press "RET"
  Then I should be in buffer "*Summary nndiscourse:emacs*"
  And I go to word "LeifCarrot"
  And I press "r"
  Then I should be in buffer "*unsent followup to LeifCarrotson on news*"
  And I type "This is a test."
  And I press "C-c C-c"
  And I switch to buffer "*sent followup to LeifCarrotson on news*"
  Then I should be in buffer "*sent followup to LeifCarrotson on news*"

@delete
Scenario: delete
  When I switch to buffer "*Summary nndiscourse:emacs*"
  And I press "c y"
  Then I should be in buffer "*Group*"
  And I scan news
  When I go to word "nndiscourse:emacs"
  And I press "RET"
  Then I should be in buffer "*Summary nndiscourse:emacs*"
  And I go to word "dickmao"
  And I press "RET"
  And I press "S c"

@submit
Scenario: submit a text which must be titled
  When I switch to buffer "*Summary nndiscourse:emacs*"
  And I press "a t"
  Then I should be in buffer "*unsent posting on news*"
  And I type "test baby test baby 123"
  And I press "M->"
  And I type "this is a test"
  And I press "C-c C-c"
  When I switch to buffer "*Messages*"
  Then I should not see "Couldn't send message via news"
