with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with FamilyTree;            use FamilyTree;
with utils;                 use utils;
with Person;                use Person;

procedure Menu is

    type TreeTuple is record
        Name : Unbounded_String;
        Tree : T_FamilyTree;
    end record;

    package TreeVector is new Ada.Containers.Vectors
       (Index_Type => Natural, Element_Type => TreeTuple);
    use TreeVector;

    ExistingTrees : TreeVector.Vector;
    CurrentTree   : TreeTuple;
    Verbosity     : Integer                   := 1;
    QuitCharacter : constant Unbounded_String := To_Unbounded_String ("q");

    type String_Access is access all Unbounded_String;

    OperationAbandonnedException : exception;
    AlreadyHasParentsException   : exception;
    DeletingRootException        : exception;

    procedure HandleInput
       (Pointer          : in String_Access; TextString : in String;
        InputType        : in T_InputType := STR; MaxInt : in Integer := -1;
        CheckKeyPresence :    Boolean     := False;
        KeyMustBePresent :    Boolean     := False)
    is
        -- GET IMPUT FROM USER
        -- Pointer : pointer to the string that is going to be modified
        -- TextString : Input prompt string
        -- InputType : Expected input type from user (either string or integer or key)
        -- MaxInt : Maximum int that can be entered by user
        -- CheckKeyPresence : decide whether to check if key is present / absent in currenttree or not
        -- KeyMustBePresent : decide whether the key has to be absent or present in currenttree
        ExitInput      : Boolean := False;
        ShowTextString : Boolean := True;
        Input          : Unbounded_String;
        TempInt        : Integer;
        Tree           : T_FamilyTree renames CurrentTree.Tree;

        procedure HandleException (ErrorText : in String) is
        begin
            New_Line;
            Put (GetColoredString (ErrorText, ERROR));
            ShowTextString := False;
        end HandleException;
    begin
        while not ExitInput loop
            begin
                if ShowTextString then
                    Put (TextString);
                end if;

                Input := To_Unbounded_String (Get_Line);

                if Input = QuitCharacter then
                    raise OperationAbandonnedException;
                else
                    --  IN CASE WHEN ASKED FOR AN INTEGER, CHECKING IF IT'S A VALID INT
                    -- THIS WILL THROW AN ERROR IF IT IS NOT A VALID INT
                    TempInt := (if (InputType = INT or InputType = KEY) then UnboundedToInteger (Input) else -1);

                    if InputType = INT then
                        -- CHECKING IF INT FALLS IN THE 1 .. MaxInt RANGE (only if MaxInt isn't the default value, -1)
                        if TempInt < 0 or
                           (MaxInt /= -1 and
                            (TempInt <= 0 or TempInt > MaxInt))
                        then
                            raise Data_Error;
                        end if;
                    elsif InputType = KEY then
                        if TempInt < 0 then
                            raise Data_Error;
                        end if;

                        if CheckKeyPresence then
                            if KeyMustBePresent then
                                if not isPresent (Tree, TempInt) then
                                    raise Absent_Key_Exception;
                                end if;
                            else
                                if isPresent (Tree, TempInt) then
                                    raise Present_Key_Exception;
                                end if;
                            end if;
                        end if;
                    elsif InputType = DATE then
                        if Input /= To_Unbounded_String("") and not IsValidDateString (Input) then
                            raise Data_Error;
                        end if;
                    end if;

                    Pointer.all := Input;
                    ExitInput   := True;
                end if;
            exception
                when Data_Error | Constraint_Error =>
                    HandleException
                       ("Saisie invalide, veuillez réessayer ('" &
                        To_String (QuitCharacter) & "' pour quitter): ");
                when Present_Key_Exception         =>
                    HandleException
                       ("La clé " & GetTrimmedInt (TempInt) &
                        " est déjà présente dans l'arbre, veuillez réessayer ('" &
                        To_String (QuitCharacter) & "' pour quitter): ");
                when Absent_Key_Exception          =>
                    HandleException
                       ("La clé " & GetTrimmedInt (TempInt) &
                        " n'est pas présente dans l'arbre, veuillez réessayer ('" &
                        To_String (QuitCharacter) & "' pour quitter): ");
            end;
        end loop;
    end HandleInput;

    function GetExistingTreesLength return Integer is
    begin
        return Integer (ExistingTrees.Length) + 1;
    end GetExistingTreesLength;

    procedure AddDefaultTree is
        Tuple : TreeTuple;
    begin
        Tuple.Name := To_Unbounded_String ("Arbre exemple");
        Tuple.Tree := GetExampleFamilyTree;
        ExistingTrees.Append (Tuple);
    end AddDefaultTree;

    procedure AddNewTree (Name : in String) is
        Tuple : TreeTuple;
    begin
        Tuple.Name := To_Unbounded_String (Name);
        Tuple.Tree := getEmptyFamilyTree;
        ExistingTrees.Append (Tuple);
    end AddNewTree;

    procedure ShowExistingTrees is
        Index : Integer := 1;
    begin
        for el of ExistingTrees loop
            Put_Line (GetTrimmedInt (Index) & ". " & To_String (el.Name));
            Index := Index + 1;
        end loop;
    end ShowExistingTrees;

    NewPerson    : T_Person;
    NewPersonKey : aliased Unbounded_String;
    FirstName    : aliased Unbounded_String;
    LastName     : aliased Unbounded_String;
    Gender       : aliased Unbounded_String;
    Birthdate    : aliased Unbounded_String;
    procedure HandleCreateNewPerson
       (CheckKeyPresence : Boolean := True; Position : in T_Position := ROOT)
    is
    begin
        New_Line;
        if Position = ROOT then
            Put_Line ("--- Ajout de l'enfant ---");
        else
            Put_Line ("--- Ajout d'un nouvel ancêtre ---");
        end if;
        New_Line;
        begin
            HandleInput
               (Pointer          => NewPersonKey'Access,
                TextString       => "Entrez la clé: ", InputType => KEY,
                CheckKeyPresence => CheckKeyPresence);
            HandleInput
               (Pointer    => FirstName'Access,
                TextString => "Entrez le prénom de la personne: ");
            HandleInput
               (Pointer    => LastName'Access,
                TextString => "Entrez le nom de la personne: ");
            HandleInput
               (Pointer    => Gender'Access,
                TextString => "Entrez le sexe de la personne: ");
            HandleInput(Pointer=> Birthdate'Access, TextString => "Entrez la date d'anniversaire de la personne (format JJ-MM-AAAA): ", InputType => DATE);

            NewPerson :=
               initPersonObj
                  (FirstName => FirstName, LastName => LastName,
                   Gender    => Gender, BirthDate => Birthdate);

            if Position /= ROOT then
                New_Line;
                Put_Line
                   (GetColoredString
                       ("Un nouvel ancêtre a été ajoutée", SUCCESS));
            end if;

        exception
            when OperationAbandonnedException =>
                New_Line;
                if Position /= ROOT then
                    Put_Line
                       (GetColoredString
                           ("Abandon de l'operation. L'ancêtre n'a pas été ajouté.",
                            WARNING));
                else
                    Put_Line
                       (GetColoredString
                           ("Abandon de l'operation. L'arbre n'a pas été crée.",
                            WARNING));
                end if;

                raise OperationAbandonnedException;
        end;
    end HandleCreateNewPerson;

    NewTreeName : aliased Unbounded_String;
    procedure HandleCreateTree is
        Tree : T_FamilyTree renames CurrentTree.Tree;
    begin
        New_Line;
        Put_Line ("--- Créer un arbre ---");
        New_Line;

        begin
            HandleInput
               (Pointer    => NewTreeName'Access,
                TextString =>
                   "Entrez le nom de l'arbre à créer ('q' pour quitter): ");

            AddNewTree (To_String (NewTreeName));

            begin
                HandleCreateNewPerson (CheckKeyPresence => False);

                CurrentTree :=
                   ExistingTrees.Element (Index => ExistingTrees.Last_Index);
                initChild (Tree, UnboundedToInteger (NewPersonKey), NewPerson);
                ExistingTrees.Replace_Element
                   (ExistingTrees.Last_Index, CurrentTree);
                New_Line;
                Put_Line
                   (GetColoredString
                       ("L'arbre " & To_String (CurrentTree.Name) &
                        " a été crée.",
                        SUCCESS));
            exception
                when OperationAbandonnedException =>
                    null;
            end;
        exception
            when OperationAbandonnedException =>
                New_Line;
                Put_Line
                   (GetColoredString
                       ("Abandon de l'operation. L'arbre n'a pas été crée.",
                        WARNING));
        end;
    end HandleCreateTree;

    NewVerbosity : aliased Unbounded_String;
    procedure HandleChangeVerbosity is
    begin
        New_Line;
        Put_Line ("--- Verbosité ---");
        New_Line;
        Put_Line
           ("Verbosité actuellement définie:" & Integer'Image (Verbosity));
        New_Line;
        Put_Line ("1. Afficher 'rôle' : 'clé'");
        Put_Line ("2. Afficher 'rôle' : 'nom et prénom (clé)'");
        Put_Line ("3. Afficher toutes les informations connues");
        Put_Line ("4. Afficher toutes les informations");
        Put_Line ("q. Retourner au menu principal");
        New_Line;

        NewVerbosity := To_Unbounded_String ("-1");
        begin
            HandleInput
               (Pointer    => NewVerbosity'Access,
                TextString =>
                   "Entrez votre choix " & GetMenuRangeString (4) & ": ",
                InputType  => INT, MaxInt => 4);

            if UnboundedToInteger (NewVerbosity) /= -1 then
                Verbosity := UnboundedToInteger (NewVerbosity);
                New_Line;
                Put_Line
                   (GetColoredString
                       ("La verbosité a été définie à" &
                        Integer'Image (Verbosity),
                        SUCCESS));
            end if;
        exception
            when OperationAbandonnedException =>
                null;
        end;
    end HandleChangeVerbosity;

    TreeMenuChoice     : aliased Unbounded_String;
    TargetKey          : aliased Unbounded_String;
    TargetGeneration   : aliased Unbounded_String;
    ShowIndividualTree : Boolean := False;
    procedure TreeMenu is
        procedure HandleShowTreeFromId is
        begin
            begin
                HandleInput
                   (Pointer          => TargetKey'Access,
                    TextString => "Entrez la clé de la personne à afficher: ",
                    InputType        => KEY, CheckKeyPresence => True,
                    KeyMustBePresent => True);
                ShowIndividualTree := True;
            exception
                when OperationAbandonnedException =>
                    New_Line;
                    Put_Line
                       (GetColoredString ("Abandon de l'operation.", WARNING));
            end;
        end HandleShowTreeFromId;

        procedure HandleAddAncestor is
            Tree           : T_FamilyTree renames CurrentTree.Tree;
            TargetPosition : T_Position;
        begin
            begin
                HandleInput
                   (Pointer          => TargetKey'Access,
                    TextString       =>
                       "Entrez la clé de la personne à qui ajouter l'ancêtre: ",
                    InputType        => KEY, CheckKeyPresence => True,
                    KeyMustBePresent => True);

                if isEmpty
                      (getLeftChild
                          (getNode (Tree, UnboundedToInteger (TargetKey))))
                then
                    TargetPosition := LEFT;
                elsif isEmpty
                      (getRightChild
                          (getNode (Tree, UnboundedToInteger (TargetKey))))
                then
                    TargetPosition := RIGHT;
                else
                    raise AlreadyHasParentsException;
                end if;

                HandleCreateNewPerson;

                addAncestor
                   (ABR => Tree, TargetKey => UnboundedToInteger (TargetKey),
                    Position  => TargetPosition,
                    NewKey    => UnboundedToInteger (NewPersonKey),
                    NewPerson => NewPerson);

                New_Line;
                Put_Line
                   (GetColoredString
                       ("L'ancêtre " & To_String (NewPersonKey) &
                        " a été ajouté à l'individu " & To_String (TargetKey) &
                        " en tant que parent " &
                        (if TargetPosition = LEFT then "1" else "2") & ".",
                        SUCCESS));
            exception
                when OperationAbandonnedException =>
                    New_Line;
                    Put_Line
                       (GetColoredString
                           ("Abandon de l'operation. La personne n'a pas été ajoutée.",
                            WARNING));
                when AlreadyHasParentsException   =>
                    New_Line;
                    Put_Line
                       (GetColoredString
                           ("L'individu " & To_String (TargetKey) &
                            " a déjà deux ancêtres.",
                            WARNING));
            end;
        end HandleAddAncestor;

        procedure HandleDeleteAncestor is
            Tree : T_FamilyTree renames CurrentTree.Tree;
        begin
            begin
                HandleInput
                   (Pointer          => TargetKey'Access,
                    TextString => "Entrez la clé de la personne à supprimer: ",
                    InputType        => KEY, CheckKeyPresence => True,
                    KeyMustBePresent => True);

                if UnboundedToInteger (TargetKey) = FamilyTree.getKey(ABR => Tree) then
                    raise DeletingRootException;
                end if;

                deleteAncestor (Tree, UnboundedToInteger (TargetKey));

                New_Line;
                Put_Line
                   (GetColoredString
                       ("L'individu " & To_String (TargetKey) &
                        " a été supprimé.",
                        SUCCESS));
            exception
                when OperationAbandonnedException =>
                    New_Line;
                    Put_Line
                       (GetColoredString
                           ("Abandon de l'operation. Aucun individu n'a été supprimé.",
                            WARNING));
                when DeletingRootException =>
                    New_Line;
                    Put_Line
                       (GetColoredString
                           ("Vous ne pouvez pas supprimer l'individu 'racine' de l'arbre.",
                            ERROR));
            end;
        end HandleDeleteAncestor;

        procedure HandleGetAncestorsCount is
            Tree           : T_FamilyTree renames CurrentTree.Tree;
            AncestorsCount : Integer;
        begin
            begin
                HandleInput
                   (Pointer          => TargetKey'Access,
                    TextString       =>
                       "Entrez la clé de la personne dont vous souhaitez obtenir le nombre d'ancêtres: ",
                    InputType        => KEY, CheckKeyPresence => True,
                    KeyMustBePresent => True);

                AncestorsCount :=
                   getAncestorsCount (Tree, UnboundedToInteger (TargetKey));
                New_Line;
                if AncestorsCount = 1 then
                    Put_Line
                       (GetColoredString
                           ("L'individu " & To_String (TargetKey) &
                            " n'a pas d'ancêtres.",
                            WARNING));
                else
                    Put_Line
                       (GetColoredString
                           ("L'individu " & To_String (TargetKey) & " a " &
                            GetTrimmedInt (AncestorsCount) &
                            " ancêtres (lui compris).",
                            SUCCESS));
                end if;
            exception
                when OperationAbandonnedException =>
                    New_Line;
                    Put_Line
                       (GetColoredString ("Abandon de l'operation.", WARNING));
            end;
        end HandleGetAncestorsCount;

        procedure HandleGetAncestorsByGeneration is
            Tree      : T_FamilyTree renames CurrentTree.Tree;
            Ancestors : FamilyTree.TreeVector.Vector;
        begin
            begin
                HandleInput
                   (Pointer    => TargetGeneration'Access,
                    TextString =>
                       "Entrez la génération N des ancêtres à rechercher: ",
                    InputType  => INT);
                HandleInput
                   (Pointer          => TargetKey'Access,
                    TextString       =>
                       "Entrez la clé de la personne dont vous souhaitez obtenir les ancêtres de génération " &
                       GetTrimmedInt (UnboundedToInteger (TargetGeneration)) &
                       ": ",
                    InputType        => KEY, CheckKeyPresence => True,
                    KeyMustBePresent => True);

                Ancestors :=
                   getAncestorsByGeneration
                      (ABR => Tree, Key => UnboundedToInteger (TargetKey),
                       Generation => UnboundedToInteger (TargetGeneration));

                New_Line;
                if Ancestors.Is_Empty then
                    Put_Line
                       (GetColoredString
                           ("L'individu " & To_String (TargetKey) &
                            " n'a pas d'ancêtres de génération " &
                            GetTrimmedInt
                               (UnboundedToInteger (TargetGeneration)) &
                            ".",
                            WARNING));
                else
                    Put_Line
                       (GetColoredString
                           ("L'individu " & To_String (TargetKey) & " a " &
                            GetKeysStringFromTreeVector
                               (Ancestors, Verbosity) &
                            " comme ancêtr" &
                            (if FamilyTree.Length (Ancestors) = 1 then "e"
                             else "es") &
                            " de génération " &
                            GetTrimmedInt
                               (UnboundedToInteger (TargetGeneration)) &
                            ".",
                            SUCCESS));
                end if;
            exception
                when OperationAbandonnedException =>
                    New_Line;
                    Put_Line
                       (GetColoredString ("Abandon de l'operation.", WARNING));
            end;
        end HandleGetAncestorsByGeneration;

        procedure HandleGetOrphanIndividuals is
            Tree              : T_FamilyTree renames CurrentTree.Tree;
            OrphanIndividuals : FamilyTree.TreeVector.Vector;
        begin
            OrphanIndividuals :=
               FamilyTree.getOrphanIndividuals (Tree, getKey (Tree));

            New_Line;
            if OrphanIndividuals.Is_Empty then
                Put_Line
                   (GetColoredString
                       ("Il n'existe aucun individu sans parents.", WARNING));
            elsif FamilyTree.Length (OrphanIndividuals) = 1 then
                Put_Line
                   (GetColoredString
                       ("L'individu " &
                        GetKeysStringFromTreeVector
                           (OrphanIndividuals, Verbosity) &
                        " est orphelin.",
                        SUCCESS));
            else
                Put_Line
                   (GetColoredString
                       ("Les individus " &
                        GetKeysStringFromTreeVector
                           (OrphanIndividuals, Verbosity) &
                        " sont orphelins.",
                        SUCCESS));
            end if;
        end HandleGetOrphanIndividuals;

        procedure HandleGetSingleParentIndividuals is
            Tree                    : T_FamilyTree renames CurrentTree.Tree;
            SingleParentIndividuals : FamilyTree.TreeVector.Vector;
        begin
            SingleParentIndividuals :=
               FamilyTree.getSingleParentIndividuals (Tree, getKey (Tree));

            New_Line;
            if SingleParentIndividuals.Is_Empty then
                Put_Line
                   (GetColoredString
                       ("Il n'existe aucun individu avec un seul parent.",
                        WARNING));
            elsif FamilyTree.Length (SingleParentIndividuals) = 1 then
                Put_Line
                   (GetColoredString
                       ("L'individu " &
                        GetKeysStringFromTreeVector
                           (SingleParentIndividuals, Verbosity) &
                        " a un seul parent.",
                        SUCCESS));
            else
                Put_Line
                   (GetColoredString
                       ("Les individus " &
                        GetKeysStringFromTreeVector
                           (SingleParentIndividuals, Verbosity) &
                        " ont un seul parent.",
                        SUCCESS));
            end if;
        end HandleGetSingleParentIndividuals;

        procedure HandleGetDualParentIndividuals is
            Tree                  : T_FamilyTree renames CurrentTree.Tree;
            DualParentIndividuals : FamilyTree.TreeVector.Vector;
        begin
            DualParentIndividuals :=
               FamilyTree.getDualParentIndividuals (Tree, getKey (Tree));

            New_Line;
            if DualParentIndividuals.Is_Empty then
                Put_Line
                   (GetColoredString
                       ("Il n'existe aucun individu avec deux parents.",
                        WARNING));
            elsif FamilyTree.Length (DualParentIndividuals) = 1 then
                Put_Line
                   (GetColoredString
                       ("L'individu " &
                        GetKeysStringFromTreeVector
                           (DualParentIndividuals, Verbosity) &
                        " a deux parents.",
                        SUCCESS));
            else
                Put_Line
                   (GetColoredString
                       ("Les individus " &
                        GetKeysStringFromTreeVector
                           (DualParentIndividuals, Verbosity) &
                        " ont deux parents.",
                        SUCCESS));
            end if;
        end HandleGetDualParentIndividuals;

        ExitTreeMenu : Boolean := False;
        Tree         : T_FamilyTree renames CurrentTree.Tree;
    begin
        while not ExitTreeMenu loop
            New_Line;
            Put_Line
               ("--- Gestion de l'arbre " & To_String (CurrentTree.Name) &
                " ---");
            New_Line;
            if ShowIndividualTree then
                showFamilyTree
                   (getNode (Tree, UnboundedToInteger (TargetKey)), Verbosity);
                ShowIndividualTree := False;
            else
                showFamilyTree (Tree, Verbosity);
            end if;
            New_Line;
            Put_Line ("1. Afficher l'arbre à partir d'un individu donné");
            Put_Line ("2. Ajouter un ancêtre");
            Put_Line ("3. Supprimer un ancêtre");
            Put_Line ("4. Obtenir le nombre d'ancêtres connus d'un individu");
            Put_Line
               ("5. Obtenir l'ensemble des ancêtres de generation N d'un individu");
            Put_Line ("6. Obtenir les individus sans parents connus");
            Put_Line ("7. Obtenir les individus avec un seul parent connu");
            Put_Line ("8. Obtenir les individus avec deux parents connu");

            begin
                HandleInput
                   (Pointer    => TreeMenuChoice'Access,
                    TextString =>
                       "Entrez votre choix " & GetMenuRangeString (8) & ": ",
                    InputType  => INT, MaxInt => 8);

                case UnboundedToInteger (TreeMenuChoice) is
                    when 1 =>
                        HandleShowTreeFromId;
                    when 2 =>
                        HandleAddAncestor;
                    when 3 =>
                        HandleDeleteAncestor;
                    when 4 =>
                        HandleGetAncestorsCount;
                    when 5 =>
                        HandleGetAncestorsByGeneration;
                    when 6 =>
                        HandleGetOrphanIndividuals;
                    when 7 =>
                        HandleGetSingleParentIndividuals;
                    when 8 =>
                        HandleGetDualParentIndividuals;
                    when others =>
                        null;
                end case;
            exception
                when OperationAbandonnedException =>
                    ExitTreeMenu := True;
            end;
        end loop;

    end TreeMenu;

    TreeOperationIndex : aliased Unbounded_String;
    procedure HandleTreeOperation (Operation : in T_OperationType) is
        TitleText : constant String :=
           (if Operation = CHOOSE then "Choisir un arbre"
            else "Supprimer un arbre");
    begin
        if GetExistingTreesLength - 1 > 0 then
            New_Line;
            Put_Line ("--- " & TitleText & " ---");
            New_Line;
            ShowExistingTrees;
            Put_Line ("q. Retourner au menu principal");
            New_Line;

            TreeOperationIndex := To_Unbounded_String ("-1");
            begin
                HandleInput
                   (Pointer    => TreeOperationIndex'Access,
                    TextString =>
                       "Entrez votre choix " &
                       GetMenuRangeString (GetExistingTreesLength - 1) & ": ",
                    InputType  => INT, MaxInt => GetExistingTreesLength - 1);

                if Operation = CHOOSE then
                    CurrentTree :=
                       ExistingTrees.Element
                          (UnboundedToInteger (TreeOperationIndex) - 1);

                    New_Line;
                    Put_Line
                       (GetColoredString
                           ("L'arbre '" & To_String (CurrentTree.Name) &
                            "' a été choisi.",
                            SUCCESS));
                    TreeMenu;
                elsif Operation = DELETE then
                    New_Line;
                    Put_Line
                       (GetColoredString
                           ("L'arbre '" &
                            To_String
                               (ExistingTrees.Element
                                   (UnboundedToInteger (TreeOperationIndex) -
                                    1)
                                   .Name) &
                            "' a été supprimé.",
						   SUCCESS));
					declare
						TreeToDelete : T_FamilyTree := ExistingTrees.Element(Index => UnboundedToInteger (TreeOperationIndex) - 1).Tree;
                    begin
                        FamilyTree.clean (ABR => TreeToDelete);
                    end;
                    Delete
                       (ExistingTrees,
                        UnboundedToInteger (TreeOperationIndex) - 1);
                end if;

            exception
                when OperationAbandonnedException =>
                    null;
            end;
        else
            New_Line;
            Put_Line
               (GetColoredString
                   ("Aucun arbre existant. Veuillez d'abord en créer un.",
                    WARNING));
        end if;
    end HandleTreeOperation;

    MainMenuChoice : aliased Unbounded_String;
begin
    AddDefaultTree;

    while True loop
        New_Line;
        Put_Line ("--- Menu principal ---");
        New_Line;
        Put_Line ("1. Choisir un arbre");
        Put_Line ("2. Créer un arbre");
        Put_Line ("3. Supprimer un arbre");
        Put_Line
           ("4. Changer la verbosité (" & GetTrimmedInt (Verbosity) &
            " actuellement)");
        Put_Line ("q. Quitter");
        New_Line;

        begin
            HandleInput
               (Pointer    => MainMenuChoice'Access,
                TextString =>
                   "Entrez votre choix " & GetMenuRangeString (4) & ": ",
                InputType  => INT, MaxInt => 4);

            case UnboundedToInteger (MainMenuChoice) is
                when 1 =>
                    HandleTreeOperation (CHOOSE);
                when 2 =>
                    HandleCreateTree;
                when 3 =>
                    HandleTreeOperation (DELETE);
                when 4 =>
                    HandleChangeVerbosity;
                when others =>
                    null;
            end case;
        exception
            when OperationAbandonnedException =>
                exit;
        end;
    end loop;
end Menu;
