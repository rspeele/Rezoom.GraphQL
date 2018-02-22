create table Users
( Id int primary key autoincrement
, Name string(64)
, Email string(256) unique
, PasswordHash binary(20)
, PasswordSalt binary(24)
);

create table Sessions
( Token string(16) primary key
, UserId int references Users(Id)
, StartedUtc datetime
);

create index IX_Sessions_UserId on Sessions(UserId);

-- file system
create table Entries
( Id int primary key autoincrement
, ParentFolderId int null references Entries(Id)
, Name string(64)
, Content string null -- null for folders
);

create index IX_Entries_ParentFolderId on Entries(ParentFolderId);

create table Permissions
( EntryId int references Entries(Id)
, UserId int references Users(Id)
, Capability int
, Grant bool
, unique(EntryId, UserId, Capability)
);
