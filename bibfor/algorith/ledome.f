      SUBROUTINE LEDOME(OPTION,NOMO  ,MATERI,MATE  ,CARELE)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/01/2012   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      CHARACTER*8  NOMO
      CHARACTER*24 MATE,CARELE
      CHARACTER*8  MATERI
      CHARACTER*2  OPTION
C
C ----------------------------------------------------------------------
C
C LECTURE DONNEES MECANIQUES
C
C ----------------------------------------------------------------------
C
C IN  OPTION : PRECISE SI MATERIAU/CARA_ELEM OBLIGATOIRES
C              ALARME L'UTILISATUER  EN CAS D'ABSENCE
C OUT NOMO   : MODELE
C OUT MATERI : CHAMP DE MATERIAU (NON CODE)
C OUT MATE   : MATERIAU CODE
C OUT CARELE : CARACTERISTIQUES ELEMENTAIRES
C
C ----------------------------------------------------------------------
C
      INTEGER      IARG,N,IBID,IRET
      CHARACTER*8  REPONS
C
C ----------------------------------------------------------------------
C
      NOMO   = ' '
      MATE   = ' '
      CARELE = ' '
C
C --- RECUPERER LE MODELE
C
      CALL GETVID(' ','MODELE',1,IARG,1,NOMO  ,N)
C
C --- RECUPERER LE MATERIAU
C
      CALL GETVID(' ','CHAM_MATER',1,IARG,1,MATERI,N)
      IF (NOMO.NE.' ') THEN
        CALL DISMOI('F','BESOIN_MATER',NOMO  ,'MODELE',IBID  ,
     &              REPONS,IRET  )
        IF ((N.EQ.0).AND.
     &      (REPONS(1:3).EQ.'OUI').AND.
     &      (OPTION(1:1).EQ.'O')) THEN
          CALL U2MESS('A','CALCULEL3_40')
        ENDIF
      ENDIF
C
C --- CREATION DE LA CARTE DU MATERIAU CODE
C
      IF (N.NE.0) THEN
        CALL RCMFMC(MATERI,MATE)
      ELSE
        MATE   = ' '
      ENDIF
C
C --- RECUPERER LES CARACTERISTIQUES ELEMENTAIRES
C
      CALL GETVID(' ','CARA_ELEM',1,IARG,1,CARELE,N)
      IF (NOMO.NE.' ') THEN
        CALL DISMOI('F'   ,'EXI_RDM'  ,NOMO  ,'MODELE',IBID,
     &              REPONS,IRET)
        IF ((N.EQ.0).AND.
     &      (REPONS(1:3).EQ.'OUI').AND.
     &      (OPTION(1:1).EQ.'O')) THEN
          CALL U2MESS('A','CALCULEL3_39')
        ENDIF
      ENDIF
C
      END
