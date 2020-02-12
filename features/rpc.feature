Scenario: install
  Given gnus start
  And I dump buffer
  Then of-record unreads for "nndiscourse+meta.discourse.org:bug" is 2
  And I go to word "bug"
  And I press "RET"
  Then I should be in buffer "*Summary nndiscourse+meta.discourse.org:bug*"
  And I go to word "david"
  And I press "RET"
  And I switch to buffer "*Article nndiscourse+meta.discourse.org:bug*"
  Then I should see "Recent Changes"
  Then prospective unreads for "nndiscourse+meta.discourse.org:bug" is 1
  And I switch to buffer "*Summary nndiscourse+meta.discourse.org:bug*"
  And I press "q"
  Then of-record unreads for "nndiscourse+meta.discourse.org:bug" is 1
