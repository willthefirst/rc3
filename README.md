Future features:

- Walls to restrict movement
    - Walls exist in the world
    - canIMoveHere SelectedUser Destination World -- function is written
         - Simple X: Full vertical wall across whole world.
            - Compare destinations X with our X, if wall is in between then can't go there.
         - Simple Y: Full horizontal wall across whole world.
            - Compare destinations Y with our Y, if wall is in between then can't go there.
         - Corner case: 
            - HARD
        - pathToLocation:
            - imagineMoveInEveryDirection
            - Use depth first search?