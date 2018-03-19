Feature: Shoplist

  Scenario: Generate shoplist from one-line recipe
    Given I am in buffer "*TestShoplist*"
    When I insert "- (200g Nuts) mahlen"
    And I call "(org-generate-shoplist)"
    Then I should see "| Nuts | 200g |"


