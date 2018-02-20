-- root folder

insert into Entries
    row ParentFolderId = null
    , Name = '/'
    , Content = null
    ;

-- password "test" for all users

insert into Users
    row Name = 'Robert'
    , Email = 'robert.s.peele@gmail.com'
    , PasswordHash = x'4974792D73907D30D19194C03385BF8D5C5C9388'
    , PasswordSalt = x'1F3EB69C06E91DF6ECB3CAB41CEDAEA9E8EAA6B4F03C5E6B'
    ;

insert into Users
    row Name = 'Test Person'
    , Email = 'test.person@example.com'
    , PasswordHash = x'4974792D73907D30D19194C03385BF8D5C5C9388'
    , PasswordSalt = x'1F3EB69C06E91DF6ECB3CAB41CEDAEA9E8EAA6B4F03C5E6B'
    ;

insert into Permissions
    row EntryId = (select Id from Entries where ParentFolderId is null)
    , UserId = (select Id from Users where Email = 'robert.s.peele@gmail.com')
    , Capability = 1
    , Grant = true
    ;

insert into Permissions
    row EntryId = (select Id from Entries where ParentFolderId is null)
    , UserId = (select Id from Users where Email = 'robert.s.peele@gmail.com')
    , Capability = 2
    , Grant = true
    ;