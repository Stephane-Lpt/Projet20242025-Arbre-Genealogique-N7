-- 1. Choisir un arbre
-- 2. Créer un arbre
-- 3. Supprimer un arbre
-- 4. Changer la verbosité (4 actuellement)
-- 5. Quitter

-- 1. Ajouter un ancêtre
-- 2. Modifier les données d'un ancêtre
-- 3. Supprimer un ancêtre
-- 4. Obtenir le nombre d'ancêtres connus d'un individu
-- 5. Obtenir l'ensemble d'ancêtres de génération N d'un individu
-- 6. Afficher l'arbre à partir d'un noeud donné
-- 7. Obtenir les individus sans parents connus
-- 8. Obtenir les individus avec un seul parent connu
-- 9. Obtenir les individus avec deux parents
-- 10. Supprimer l'arbre
-- 11. Quitter

-- BIG TODO : handle case when user puts "1 2" when asked for prompt

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with FamilyTree;            use FamilyTree;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with utils; use utils;
with Person; use Person;

procedure Menu is

    type TreeTuple is record
        Name: Unbounded_String;
        Tree: T_FamilyTree;
    end record;

    package TreeVector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => TreeTuple);
    use TreeVector;

    ExistingTrees  : TreeVector.Vector;
    CurrentTree    : TreeTuple;
    Verbosity      : Integer := 1;
    QuitCharacter  : constant Unbounded_String := To_Unbounded_String ("q");

    type String_Access is Access all Unbounded_String;
    type Boolean_Access is Access all Boolean;
    type Person_Access is Access all T_Person;

    OperationAbandonnedException : exception;

    
    TreeMenuChoice : aliased Unbounded_String;
    procedure TreeMenu is
        procedure HandleAddAncestor is
        begin
            null;
        end HandleAddAncestor;

        procedure HandleModifyAncestor is
        begin
            null;
        end HandleModifyAncestor;

        procedure HandleDeleteAncestor is
        begin
            null;
        end HandleDeleteAncestor;

        procedure HandleGetAncestorsCount is
        begin
            null;
        end HandleGetAncestorsCount;

        procedure HandleGetAncestorsByGeneration is
        begin
            null;
        end HandleGetAncestorsByGeneration;

        procedure HandleShowTree is
        begin
            null;
        end HandleShowTree;

        procedure HandleGetOrphanIndividuals is
        begin
            null;
        end HandleGetOrphanIndividuals;

        procedure HandleGetSingleParentIndividuals is
        begin
            null;
        end HandleGetSingleParentIndividuals;

        procedure HandleGetDualParentIndividuals is
        begin
            null;
        end HandleGetDualParentIndividuals;

        Choice : Integer;
        ExitTreeMenu : Boolean := False;

    begin
        while not ExitTreeMenu loop
            New_Line;
            Put_Line ("--- Gestion de l'arbre " & To_String(CurrentTree.Name) & " ---");
            New_Line;
            showFamilyTree (CurrentTree.Tree, Verbosity => 2);
            New_Line;
            Put_Line ("1. Afficher l'arbre avec la verbosité " & getTrimmedInt(Verbosity));
            Put_Line ("2. Afficher l'arbre à partir d'un noeud donne");
            Put_Line ("3. Ajouter un ancêtre");
            Put_Line ("4. Supprimer un ancêtre");
            Put_Line ("5. Obtenir le nombre d'ancêtres connus d'un individu");
            Put_Line ("6. Obtenir l'ensemble des ancêtres de generation N d'un individu");
            Put_Line ("7. Obtenir les individus sans parents connus");
            Put_Line ("8. Obtenir les individus avec un seul parent connu");
            Put_Line ("9. Obtenir les individus avec deux parents");
            
            begin
                HandleInput
            New_Line;
            Put ("Entrez votre choix (1-11) : ");
            Get (Item => Choice);

            case Choice is
                when 1 =>
                    showFamilyTree (CurrentTree.Tree, Verbosity);
                when 2 =>
                    Null;
                when 3 =>
                    HandleAddAncestor;
                when 4 =>
                    HandleGetAncestorsCount;
                when 5 =>
                    HandleGetAncestorsByGeneration;
                when 6 =>
                    HandleShowTree;
                when 7 =>
                    HandleGetOrphanIndividuals;
                when 8 =>
                    HandleGetSingleParentIndividuals;
                when 9 =>
                    HandleGetDualParentIndividuals;
                when 11 =>
                    ExitTreeMenu := True;
                when others =>
                    Put_Line ("Choix invalide, veuillez réessayer (1-11).");
            end case;
        end loop;

    end TreeMenu;
    
    procedure HandleInput(Pointer : in String_Access; TextString : in String; InputType : in T_InputType := STR; MaxInt : in Integer := -1; CheckKeyPresence : Boolean := False; KeyMustBePresent : Boolean := False) is
        -- GET IMPUT FROM USER
        -- Pointer : pointer to the string that is going to be modified
        -- TextString : Input prompt string 
        -- InputType : Expected input type from user (either string or integer or key)
        -- MaxInt : Maximum int that can be entered by user
        -- CheckKeyPresence : decide whether to check if key is present / absent in currenttree or not
        -- KeyMustBePresent : decide whether the key has to be absent or present in currenttree
        ExitInput : Boolean := False;
        ShowTextString : Boolean := True;
        Input : Unbounded_String;
        TempInt : Integer;

        procedure HandleException(ErrorText : in String) is 
        begin
            New_Line;
            Put(getColoredString(ErrorText, ERROR));
            ShowTextString := False;
        end HandleException;
    begin
        while not ExitInput loop
            begin
                if ShowTextString then
                    Put (TextString);
                end if;

                Input := To_Unbounded_String(Get_Line);

                if Input = QuitCharacter then
                    raise OperationAbandonnedException;
                else
                    --  IN CASE WHEN ASKED FOR AN INTEGER, CHECKING IF IT'S A VALID INT
                    TempInt := (if (InputType = INT or InputType = KEY) then Integer'Value(To_String(Input)) else -1);

                    if InputType = INT then
                        -- CHECKING IF INT FALLS IN THE 1 .. MaxInt RANGE (only if MaxInt isn't the default value, -1)
                        if MaxInt /= -1 and (TempInt <= 0 or TempInt > MaxInt) then
                            raise Data_Error;
                        end if;
                    elsif InputType = KEY then
                        if CheckKeyPresence then
                            declare 
                                Tree: T_FamilyTree renames CurrentTree.Tree;
                            begin
                                if KeyMustBePresent then
                                    if not isPresent(Tree, TempInt) then
                                        raise Absent_Key_Exception;
                                    end if;
                                else
                                    if isPresent(Tree, TempInt) then
                                        raise Present_Key_Exception;
                                    end if;
                                end if;
                            end;
                        end if;
                    end if;

                    Pointer.all := Input;
                    ExitInput := True;
                end if;
            exception
                when Data_Error | Constraint_Error =>
                    HandleException("Saisie invalide, veuillez réessayer ('" & To_String(QuitCharacter) & "' pour quitter): ");
                when Present_Key_Exception =>
                    HandleException("La clé " & Integer'Image(TempInt) & " est déjà présente dans l'arbre, veuillez réessayer ('" & To_String(QuitCharacter) & "' pour quitter): ");
                when Absent_Key_Exception =>
                    HandleException("La clé " & Integer'Image(TempInt) & " n'est pas présente dans l'arbre, veuillez réessayer ('" & To_String(QuitCharacter) & "' pour quitter): ");
            end;
        end loop;
    end HandleInput;

    function GetExistingTreesLength return Integer is
    begin
        return Integer(ExistingTrees.Length) + 1;
    end GetExistingTreesLength;

    procedure AddDefaultTree is
        Tuple : TreeTuple;
    begin
        Tuple.Name := To_Unbounded_String("Arbre exemple");
        Tuple.Tree := FamilyTree.GetExampleFamilyTree;
        ExistingTrees.Append (Tuple);
    end AddDefaultTree;
    
    procedure AddNewTree(Name : in String) is
        Tuple : TreeTuple;
    begin
        Tuple.Name := To_Unbounded_String(Name);
        Tuple.Tree := getEmptyFamilyTree;
        ExistingTrees.Append (Tuple);
    end AddNewTree;

    procedure ShowExistingTrees is
        Index : Integer := 1;
    begin
        for el of ExistingTrees loop
            Put_Line (getTrimmedInt(Index) & ". " & To_String(el.Name));
            Index := Index + 1;
        end loop;
    end ShowExistingTrees;

    NewPerson : aliased T_Person;
    NewPersonKey : aliased Unbounded_String;
    FirstName : aliased Unbounded_String;
    LastName : aliased Unbounded_String;
    Gender : aliased Unbounded_String;
    Birthdate : aliased Unbounded_String;
    procedure HandleCreateNewPerson(Position : in T_Position := ROOT) is
    begin
        New_Line;
        if Position = ROOT then
            Put_Line ("--- Ajout de l'enfant ---");
        else
            Put_Line ("--- Ajout d'un nouvel ancêtre ---");
        end if;
        New_Line;
        begin
            HandleInput (Pointer => NewPersonKey'Access, TextString => "Entrez la clé: ", InputType => KEY, CheckKeyPresence => False);
            HandleInput (Pointer => FirstName'Access, TextString => "Entrez le prénom de la personne: ");
            HandleInput (Pointer => LastName'Access, TextString => "Entrez le nom de la personne: ");
            HandleInput (Pointer => Gender'Access, TextString => "Entrez le sexe de la personne: ");
            HandleInput (Pointer => Birthdate'Access, TextString => "Entrez la date d'anniversaire de la personne: ");
            
            NewPerson := initPersonObj(
                FirstName => FirstName, 
                LastName => LastName, 
                Gender => Gender, 
                BirthDate => Birthdate
            );

            if Position /= ROOT then
                New_Line;
                Put_Line(getColoredString("Un nouvel ancêtre a été ajoutée", SUCCESS));
            end if;

        exception
            when OperationAbandonnedException =>
                New_Line;
                if Position /= ROOT then
                    Put_Line(getColoredString("Abandon de l'operation. L'ancêtre n'a pas été ajouté.", WARNING));
                else
                    Put_Line(getColoredString("Abandon de l'operation. L'arbre n'a pas été crée.", WARNING));
                end if;

                raise OperationAbandonnedException;
        end;
    end HandleCreateNewPerson;

    TreeOperationIndex : aliased Unbounded_String;
    procedure HandleTreeOperation(Operation : in T_OperationType) is
        TitleText : String := (if Operation = CHOOSE then "Choisir un arbre" else "Supprimer un arbre");
    begin
        if GetExistingTreesLength - 1 > 0 then
            New_Line;
            Put_Line ("--- " & TitleText & " ---");
            New_Line;
            ShowExistingTrees;
            Put_Line ("q. Retourner au menu principal");
            New_Line;

            TreeOperationIndex := To_Unbounded_String("-1");
            begin
                HandleInput (Pointer => TreeOperationIndex'Access, TextString => "Entrez votre choix " & getMenuRangeString (GetExistingTreesLength) & ": ", InputType => INT, MaxInt => GetExistingTreesLength - 1);

                if Operation = CHOOSE then
                    CurrentTree := ExistingTrees.Element (Integer'Value(To_String(TreeOperationIndex)) - 1);

                    New_Line;
                    Put_Line(getColoredString("L'arbre '" & To_String(CurrentTree.Name) & "' a été choisi.", SUCCESS)); 
                    TreeMenu;
                elsif OPERATION = DELETE then
                    --  TODO : delete memory from this tree ( stephane ? )

                    New_Line;
                    Put_Line(getColoredString("L'arbre '" & To_String(ExistingTrees.Element(Integer'Value(To_String(TreeOperationIndex)) - 1).Name) & "' a été supprimé.", SUCCESS));
                    Delete(ExistingTrees, Integer'Value(To_String(TreeOperationIndex)) - 1);
                end if;
            
            exception
                when OperationAbandonnedException =>
                    Null;
            end;
        else
            New_Line;
            Put_Line(getColoredString("Aucun arbre existant. Veuillez d'abord en créer un.", WARNING));
        end if;
    end HandleTreeOperation;

    NewTreeName : aliased Unbounded_String;
    procedure HandleCreateTree is
        Tree: T_FamilyTree renames CurrentTree.Tree;
    begin
        New_Line;
        Put_Line ("--- Créer un arbre ---");
        New_Line;

        begin
            HandleInput (Pointer => NewTreeName'Access, TextString => "Entrez le nom de l'arbre à créer ('q' pour quitter): ");
        
            AddNewTree (To_String(NewTreeName));

            begin
                HandleCreateNewPerson;

                CurrentTree := ExistingTrees.Element(Index => ExistingTrees.Last_Index);
                initChild (Tree, Integer'Value(To_String(NewPersonKey)), NewPerson);
                ExistingTrees.Replace_Element(ExistingTrees.Last_Index, CurrentTree);
                New_Line;
                Put_Line(getColoredString("L'arbre " & To_String(CurrentTree.Name) & " a été crée.", SUCCESS));
            exception
                when OperationAbandonnedException =>
                    Null;
            end;
        exception
            when OperationAbandonnedException =>
                New_Line;
                Put_Line(getColoredString("Abandon de l'operation. L'arbre n'a pas été crée.", WARNING));
        end;
    end HandleCreateTree;
	
    NewVerbosity : aliased Unbounded_String;
    procedure HandleChangeVerbosity is
    begin
        New_Line;
        Put_Line ("--- Verbosité ---");
        New_Line;
        Put_Line("Verbosité actuellement définie:" & Integer'Image (Verbosity));
        New_Line;
        Put_Line ("1. Afficher 'rôle' : 'clé'");
        Put_Line ("2. Afficher 'rôle' : 'nom et prénom'");
        Put_Line ("3. Afficher toutes les informations connues");
        Put_Line ("4. Afficher toutes les informations");
        Put_Line ("q. Retourner au menu principal");
        New_Line;

	    NewVerbosity := To_Unbounded_String("-1");
        begin
            HandleInput (Pointer => NewVerbosity'Access, TextString => "Entrez votre choix " & getMenuRangeString (4) & ": ", InputType => INT, MaxInt => 4);

            if Integer'Value(To_String(NewVerbosity)) /= -1 then
                Verbosity := Integer'Value(To_String(NewVerbosity));
                New_Line;
                Put_Line (getColoredString("La verbosité a été définie à" & Integer'Image(Verbosity), SUCCESS));
            end if;
        exception
            when OperationAbandonnedException =>
                Null;
        end;
    end HandleChangeVerbosity;

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
        Put_Line ("4. Changer la verbosité (" & getTrimmedInt(Verbosity) & " actuellement)");
        Put_Line ("q. Quitter");
        New_Line;

        begin
            HandleInput (Pointer => MainMenuChoice'Access, TextString => "Entrez votre choix " & getMenuRangeString (4) & ": ", InputType => INT, MaxInt => 4);

            case Integer'Value(To_String(MainMenuChoice)) is
                when 1 =>
                    HandleTreeOperation (CHOOSE);
                when 2 =>
                    HandleCreateTree;
                when 3 =>
                    HandleTreeOperation (DELETE);
                when 4 =>
                    HandleChangeVerbosity;
                when others =>
                    Null;
            end case;
        exception
            when OperationAbandonnedException =>
                exit;
        end;
    end loop;
end Menu;
