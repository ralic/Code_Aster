      SUBROUTINE DISMES(QUESTI,NOMOBZ,REPI,REPKZ,IERD)
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      INTEGER             REPI, IERD
      CHARACTER*(*) QUESTI,NOMOBZ,REPKZ
C RESPONSABLE PELLET J.PELLET
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C     --     DISMOI(CHAM_ELEM_S)
C
C       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
C       NOMOBZ : NOM D'UN OBJET DE TYPE LIGREL
C
C OUT : REPI   : REPONSE ( SI ENTIERE )
C       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
C       IERD   : CODE RETOUR (0--> OK, -1 --> CHAMP INEXISTANT)
C
C ----------------------------------------------------------------------
C
      INTEGER       IBID, IRET, GD, JCESD, JCELK, JCESK
      CHARACTER*8    NOGD
      CHARACTER*19  NOMOB
      CHARACTER*24 QUESTL,K24
      CHARACTER*32  REPK
C DEB-------------------------------------------------------------------

      CALL JEMARQ()

      IERD   = 0
      NOMOB  = NOMOBZ
      REPK   = ' '
      QUESTL = QUESTI

      CALL JEEXIN ( NOMOB//'.CESD', IRET )
      IF ( IRET .EQ. 0 ) THEN
          IERD = 1
          GOTO 9999
      END IF

      CALL JEVEUO ( NOMOB//'.CESD', 'L', JCESD )
      CALL JEVEUO ( NOMOB//'.CESK', 'L', JCESK )
      NOGD = ZK8(JCESK-1+2)
      CALL JENONU(JEXNOM('&CATA.GD.NOMGD',NOGD),GD )

      IF ( QUESTI .EQ. 'TYPE_CHAMP' ) THEN
         IF (ZK8(JCESK-1+3).EQ.'ELEM') THEN
           REPK = 'CESE'
         ELSEIF (ZK8(JCESK-1+3).EQ.'ELGA') THEN
           REPK = 'CESG'
         ELSEIF (ZK8(JCESK-1+3).EQ.'ELNO') THEN
           REPK = 'CESN'
         ELSE
           CALL ASSERT(.FALSE.)
         ENDIF

      ELSEIF ( QUESTI .EQ. 'NOM_MAILLA') THEN
         REPK=ZK8(JCESK-1+1)

      ELSEIF ( QUESTL(1:6) .EQ. 'NUM_GD' ) THEN
         REPI = GD

      ELSEIF ( QUESTL(1:6) .EQ. 'NOM_GD' ) THEN
         REPK = NOGD

      ELSEIF ( QUESTI .EQ. 'MXNBSP' ) THEN
        REPI=ZI(JCESD-1+4)

      ELSEIF ( QUESTI .EQ. 'TYPE_SCA' ) THEN
         CALL DISMGD(QUESTI,NOGD, REPI, REPK, IERD )

      ELSE
         IERD = 1
      ENDIF

 9999 CONTINUE
      REPKZ = REPK

      CALL JEDEMA()
      END
