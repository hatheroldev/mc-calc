Feature: Work with vectors

  Scenario: Grab simple elements as vector in calc
    Given I insert:
    """
    1
    2
    """

    When I go to beginning of buffer
    And I call "mc/mark-next-lines"
    Then I should have 2 cursors

    When I set the mark
    And I call "end-of-line"
    And I call "mc-calc-grab"
    Then I should be in buffer "*Calculator*"

    When I type "5"
    And I type "+"
    And I call "mc-calc-copy-to-buffer"
    Then I should be in buffer "mc-calc-test"
    And I should see:
    """
    6
    7
    """
    And the buffer "*Calculator*" should not be visible

  Scenario: Grab elements with unit as vector in calc
    Given I insert:
    """
    1 ms
    2 ms
    """

    When I go to beginning of buffer
    And I call "mc/mark-next-lines"
    Then I should have 2 cursors

    When I set the mark
    And I call "end-of-line"
    And I call "mc-calc-grab"
    Then I should be in buffer "*Calculator*"

    When I type "'5 ms"
    And I type "VM+"
    And I call "mc-calc-copy-to-buffer"
    Then I should be in buffer "mc-calc-test"
    And I should see:
    """
    6 ms
    7 ms
    """
    And the buffer "*Calculator*" should not be visible

  Scenario: Calc ignores options for pop and push
    Given I insert:
    """
    4
    5
    """

    When I call "c-mode"
    And I go to beginning of buffer
    And I call "mc/mark-next-lines"
    Then I should have 2 cursors

    When I set mc-calc-major-mode-eval-options-alist to ((c-mode . (calc-language c calc-word-size 32 calc-leading-zeros t)))
    And I set mc-calc-eval-options to (calc-number-radix 16)
    And I set the mark
    And I call "end-of-line"
    And I call "mc-calc-grab"
    Then I should be in buffer "*Calculator*"

    When I type "5"
    And I type "+"
    And I call "mc-calc-copy-to-buffer"
    Then I should be in buffer "mc-calc-test"
    And I should see:
    """
    9
    10
    """
    And the buffer "*Calculator*" should not be visible

  Scenario: Substitute each cursor with single calc element
    Given I insert:
    """
    1
    2
    """

    When I go to beginning of buffer
    And I call "mc/mark-next-lines"
    Then I should have 2 cursors

    When I set the mark
    And I call "end-of-line"
    And I call "mc-calc-grab"
    Then I should be in buffer "*Calculator*"

    When I type "VR+"
    And I call "mc-calc-copy-to-buffer"
    Then I should be in buffer "mc-calc-test"
    And I should see:
    """
    3
    3
    """
    And the buffer "*Calculator*" should not be visible

  Scenario: Get vector from calc
    Given I insert:
    """
    x
    x
    """

    When I go to beginning of buffer
    And I call "mc/mark-next-lines"
    Then I should have 2 cursors

    When I set the mark
    And I call "end-of-line"
    And I call "mc-calc"
    Then I should be in buffer "*Calculator*"

    When I type "'index(mccursors)"
    And I type "="
    And I call "mc-calc-copy-to-buffer"
    Then I should be in buffer "mc-calc-test"
    And I should see:
    """
    1
    2
    """
    And the buffer "*Calculator*" should not be visible

  Scenario: Let the calculator open
    Given I call "calc"
    And I pop to buffer "mc-calc-test"
    And I insert:
    """
    x
    x
    """
    When I go to beginning of buffer
    And I call "mc/mark-next-lines"
    Then I should have 2 cursors

    When I set the mark
    And I call "end-of-line"
    And I call "mc-calc"
    Then I should be in buffer "*Calculator*"

    When I type "'index(mccursors)"
    And I type "="
    And I call "mc-calc-copy-to-buffer"
    Then I should be in buffer "mc-calc-test"
    And I should see:
    """
    1
    2
    """
    And the buffer "*Calculator*" should be visible
