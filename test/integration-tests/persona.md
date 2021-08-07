# Persona

This file is meant to describe some persona used to test the application in a user-oriented way.

1. The Very First User

    This user is the user that connects the first to a new empty instance of the application (with no data already stored).

    Theirs expectations are to be able to:
    1. create new items (notes for now) and see them displayed after creation
    2. see previously created items on a new connection
    3. delete created items and not seeing them on screen anymore
    4. modify previously created items and see the changes on screen
    5. deleted notes should be found back in trash
    6. trashed items can be force-deleted
    
    All those characteristics should be persitent after a server restart (application server/physical server)
