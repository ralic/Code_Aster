      SUBROUTINE ULDEFI( UNIT, FICNOM, DDNOM, TYPF, ACCES, AUTOR )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 31/05/2005   AUTEUR MCOURTOI M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY  
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY  
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR     
C (AT YOUR OPTION) ANY LATER VERSION.                                   
C                                                                       
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT   
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF            
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU      
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.                              
C                                                                       
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE     
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,         
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE D6BHHJP J.P.LEFEBVRE
      IMPLICIT NONE
      INTEGER            UNIT
      CHARACTER*(*)            FICNOM, DDNOM, TYPF, ACCES, AUTOR
C     ------------------------------------------------------------------
C     DEFINITION DE LA CORRESPONDANCE UN NOM UTILISATEUR ET UN NUMERO
C     D'UNITE LOGIQUE
C     ------------------------------------------------------------------
C IN  UNIT   : IS    : NUMERO D'UNITE LOGIQUE
C IN  FICNOM : CH*(*): NOM DU FICHIER ASSOCIE (FICHIER DE TYPE LIBRE
C                      UNIQUEMENT)
C IN  DDNOM  : CH*16 : NOM ASSOCIE AU NUMERO D'UNITE LOGIQUE UNIT
C IN  TYPF   : CH*1  : A -> ASCII, B -> BINAIRE, L -> LIBRE
C IN  ACCES  : N -> NEW, O -> OLD, A -> APPEND
C IN  AUTOR  : O-> AUTORISE A LA MODIFICATION, N-> N'AUTORISE PAS
C
C     ------------------------------------------------------------------
C     CONVENTION : SI UNIT <= 0 ALORS ON RETIRE LE NOM "NAME" DES TABLES
C     ------------------------------------------------------------------
C     REMARQUE : LORSQUE LE FICHIER EST DE TYPE A (ASCII) UN OPEN EST
C                REALISE PAR LA COMMANDE ULOPEN
C     ------------------------------------------------------------------
C     LIMITATION :  ON NE PEUT DEFINIR SIMULTANEMENT QUE (MXF=100)
C                   CORRESPONDANCE
C     ------------------------------------------------------------------
C     REMARQUE : SI L'INITIALISATION N'A PAS ETE FAITE LA ROUTINE S'EN
C                CHARGERA (APPEL A ULINIT)
C
C     DESCRIPTION DU COMMUN UTILISE :
C         NAMEFI = NOM DU FICHIER (255 CARACTERES MAXIMUM)
C         TYPEFI = TYPE DE FICHIER A -> ASCII , B -> BINAIRE
C         ACCEFI = TYPE D'ACCES  N -> NEW, O -> OLD, A -> APPEND
C         UNITFI = NUMERO D'UNITE LOGIQUE FORTRAN ASSOCIEE
C         ETATFI = O -> OUVERT PAR OPEN FORTRAN, ? -> INCONNU
C         MODIFI = O -> MODIFIABLE PAR L'UTILISATEUR, N -> NON 
C     ------------------------------------------------------------------
C
      INTEGER          MXF
      PARAMETER       (MXF=100)
      CHARACTER*1      TYPEFI(MXF),ACCEFI(MXF),ETATFI(MXF),MODIFI(MXF)
      CHARACTER*16     DDNAME(MXF)
      CHARACTER*255    NAMEFI(MXF)
      INTEGER          FIRST, UNITFI(MXF) , NBFILE
      COMMON/ ASGFI1 / FIRST, UNITFI      , NBFILE
      COMMON/ ASGFI2 / NAMEFI,DDNAME,TYPEFI,ACCEFI,ETATFI,MODIFI
C
      CHARACTER*255    NAMELL
      CHARACTER*16     NAME16,NOFI
      CHARACTER*8      K8B 
      CHARACTER*1      K1TYP,K1ACC,K1AUT
      INTEGER          IFILE,ILIBRE
C     ------------------------------------------------------------------
C
C     --- INITIALISATION (SI NECESSAIRE) ---
      IF ( FIRST .NE. 17111990 ) CALL ULINIT
C
      NAMELL = FICNOM
      NAME16 = DDNOM
      K1TYP  = TYPF
      K1ACC  = ACCES
      K1AUT  = AUTOR

      IF ( UNIT .LT. 0 ) THEN
C       --- ON APPELLE ULOPEN POUR LA FERMETURE
          CALL ULOPEN ( UNIT, NAMELL, NAME16, ACCES , K1AUT)
C
      ELSE
C       --- INSERTION DEMANDEE ---
        IF ( K1TYP .NE. 'A' .AND. K1TYP .NE. 'B' 
     &                      .AND. K1TYP .NE. 'L') THEN
          CALL UTMESS ('F','ULDEFI02','ARGUMENT D''APPEL INVALIDE : '
     &               //' TYPF = '//K1TYP)
        ENDIF
        IF (K1ACC.NE.'O' .AND. K1ACC.NE.'N' .AND. K1ACC.NE.'A') THEN
          CALL UTMESS ('F','ULDEFI03','ARGUMENT D''APPEL INVALIDE : '
     &               //' ACCES = '//K1ACC)
        ENDIF
        IF ( K1AUT .NE. 'O' .AND. K1AUT .NE. 'N' ) THEN
          CALL UTMESS ('F','ULDEFI04','ARGUMENT D''APPEL INVALIDE : '
     &               //' AUTOR = '//K1AUT)
        ENDIF
        IF ( K1TYP .EQ. 'A' ) THEN
C
C --- SI LE FICHIER EST DE TYPE ASCII, ON FAIT UN ULOPEN
          CALL ULOPEN ( UNIT, NAMELL, NAME16, ACCES , K1AUT)
        ELSE
          ILIBRE = 0
          DO 20 IFILE = 1, NBFILE
            IF ( DDNAME(IFILE) .EQ. NAME16 ) THEN
C
C --- ASSOCIATION DEJA EFFECTUEE, ON AUTORISE LA REDEFINITION POUR 
C     LE TYPE "LIBRE"
              IF ( K1TYP .EQ. 'L' ) THEN 
                UNITFI(IFILE) = UNIT
                ILIBRE=IFILE
              ELSE IF ( UNITFI(IFILE) .NE. UNIT ) THEN
                WRITE(K8B,'(I4)') UNIT 
                CALL UTMESS ('F','ULDEFI05','REDEFINITION DE L''UNITE '
     &                     //'LOGIQUE '//K8B//' NON AUTORISEE') 
              ENDIF
              GOTO 21
            ELSE IF (DDNAME(IFILE) .EQ. ' ' .AND. 
     &               NAMEFI(IFILE) .EQ. ' '       ) THEN
C           --- RECHERCHE DE LA DERNIERE PLACE LIBRE ---
              ILIBRE = IFILE
            ENDIF
  20      CONTINUE
          IF ( ILIBRE .EQ. 0 ) THEN
            NBFILE = NBFILE + 1
            IF (NBFILE.GT. MXF) THEN
              WRITE(K8B,'(I4)') MXF  
              CALL UTMESS ('F','ULDEFI06','NOMBRE MAXIMUM D''UNITES '//
     &                     'LOGIQUES OUVERTES ATTEINT '//K8B) 
            ENDIF 
            ILIBRE = NBFILE
          ENDIF
C
  21      CONTINUE
          IF ( FICNOM(1:1) .EQ. ' ' ) THEN
             CALL CODENT ( UNIT, 'G', K8B )
             NAMELL = 'fort.'//K8B
          ELSE
             NAMELL = FICNOM
          ENDIF
C  
          NAMEFI(ILIBRE) = NAMELL
          DDNAME(ILIBRE) = NAME16
          UNITFI(ILIBRE) = UNIT
          TYPEFI(ILIBRE) = K1TYP
          ACCEFI(ILIBRE) = K1ACC
          ETATFI(ILIBRE) = 'O'
          MODIFI(ILIBRE) = K1AUT
        ENDIF
C        
C ----
      ENDIF
C
      END
