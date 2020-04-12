Feature: Replace region with calc eval

  Scenario: Evaluate lines
    Given I insert:
    """
    1 + 10
    2 + 10
    """

    When I go to beginning of buffer
    And I call "mc/mark-next-like-this"
    Then I should have 2 cursors

    When I set the mark
    And I call "end-of-line"
    Then I should have 2 cursors

    When I call "mc-calc-eval"
    Then I should see:
    """
    11
    12
    """
    And the buffer "*Calculator*" should not be visible

  Scenario: Use cursor index
    Given I insert:
    """
    $
    $
    """

    When I go to beginning of buffer
    And I call "mc/mark-next-like-this"
    Then I should have 2 cursors

    When I set the mark
    And I call "end-of-line"
    Then I should have 2 cursors

    When I call "mc-calc-eval"
    Then I should see:
    """
    0
    1
    """
    And the buffer "*Calculator*" should not be visible

  Scenario: Use cursor count
    Given I insert:
    """
    $$
    $$
    """

    When I go to beginning of buffer
    And I call "mc/mark-next-like-this"
    Then I should have 2 cursors

    When I set the mark
    And I call "end-of-line"
    Then I should have 2 cursors

    When I call "mc-calc-eval"
    Then I should see:
    """
    2
    2
    """
    And the buffer "*Calculator*" should not be visible

  Scenario: Set calc options
    Given I insert:
    """
    10#9
    10#10
    """

    When I go to beginning of buffer
    And I call "mc/mark-next-like-this"
    And I set the mark
    And I call "end-of-line"
    Then I should have 2 cursors

    When I set mc-calc-eval-options to (calc-number-radix 16)
    And I call "mc-calc-eval"
    Then I should see:
    """
    16#9
    16#A
    """
    And the buffer "*Calculator*" should not be visible

  Scenario: Set several calc options
    Given I insert:
    """
    9
    10
    """

    When I go to beginning of buffer
    And I call "mc/mark-next-like-this"
    And I set the mark
    And I call "end-of-line"
    Then I should have 2 cursors

    When I set mc-calc-eval-options to (calc-number-radix 16 calc-language pascal)
    And I call "mc-calc-eval"
    Then I should see:
    """
    $9
    $A
    """
    And the buffer "*Calculator*" should not be visible

  Scenario: Set calc options and automatic options derived from mode
    Given I insert:
    """
    9
    10
    """

    When I call "c-mode"
    And I go to beginning of buffer
    And I call "mc/mark-next-like-this"
    And I set the mark
    And I call "end-of-line"
    Then I should have 2 cursors

    When I set mc-calc-major-mode-eval-options-alist to ((c-mode . (calc-word-size 32 calc-leading-zeros t)))
    When I set mc-calc-eval-options to (calc-number-radix 16)
    And I call "mc-calc-eval"
    Then I should see:
    """
    16#00000009
    16#0000000A
    """
    And the buffer "*Calculator*" should not be visible

  Scenario: Set calc options and no automatic options derived from mode
    Given I insert:
    """
    9
    10
    """

    When I call "c-mode"
    And I go to beginning of buffer
    And I call "mc/mark-next-like-this"
    And I set the mark
    And I call "end-of-line"
    Then I should have 2 cursors

    When I set mc-calc-major-mode-eval-options-alist to ((ruby-mode . (calc-word-size 32 calc-leading-zeros t)))
    When I set mc-calc-eval-options to (calc-number-radix 16)
    And I call "mc-calc-eval"
    Then I should see:
    """
    16#9
    16#A
    """
    And the buffer "*Calculator*" should not be visible

  Scenario: Let the calculator open
    Given I call "calc"
    And I pop to buffer "mc-calc-test"
    And I insert:
    """
    1 + 5
    2 + 7
    """

    When I go to beginning of buffer
    And I call "mc/mark-next-like-this"
    Then I should have 2 cursors

    When I set the mark
    And I call "end-of-line"
    Then I should have 2 cursors

    When I call "mc-calc-eval"
    Then I should see:
    """
    6
    9
    """
    And the buffer "*Calculator*" should be visible
