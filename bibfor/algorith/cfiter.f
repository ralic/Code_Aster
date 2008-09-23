      SUBROUTINE CFITER(RESOCO,ACCES,TYPOPE,VALI,VALR)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/09/2008   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE MABBAS M.ABBAS
C
      IMPLICIT     NONE
      CHARACTER*24 RESOCO
      CHARACTER*1  ACCES
      CHARACTER*4  TYPOPE
      INTEGER      VALI
      REAL*8       VALR
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODES DISCRETES - UTILITAIRE)
C
C MISE A JOUR DU VECTEUR DIAGNOSTIC
C
C ----------------------------------------------------------------------
C
C
C MISE A JOUR DU VECTEUR DIAGNOSTIC
C
C IN  RESOCO  : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C IN  ACCES   : TYPE D'ACCES
C                'I': INITIALISATION
C                'L': LECTURE DONNEE
C                'E': ECRITURE DONNEE
C IN  TYPOPE  : OPERATION SUR LE VECTEUR DIAGNOSTIC
C                'CONT' - COMPTEUR BOUCLE CONTACT
C                  'CONC' - CUMULE SUR PAS DE TEMPS
C                'GEOM' - COMPTEUR BOUCLE GEOMETRIE
C                'FROT' - COMPTEUR BOUCLE FROTTEMENT
C                'LIAC' - COMPTEUR LIAISONS DE CONTACT
C                'LIAF' - COMPTEUR LIAISONS DE FROTTEMENT
C                'TIMA' - TEMPS ALGORITHME DE RESOLUTION
C                'TIMG' - TEMPS ALGORITHME D'APPARIEMENT GEOMETRIQUE



C I/O  VALI   : VALEUR ENTIERE A LIRE OU ECRIRE
C I/O  VALR   : VALEUR REELLE A LIRE OU ECRIRE
C
C --------------- DEBUT DECLARATIONS NORMALISEES JEVEUX ---------------
C
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C

      INTEGER      ZDIAG
      PARAMETER    (ZDIAG=10)
      CHARACTER*24 DIAGI,DIAGT
      INTEGER      JDIAGI,JDIAGT
      INTEGER      I
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- ACCES OBJETS
C      
      DIAGI  = RESOCO(1:14)//'.DIAG.ITER'
      DIAGT  = RESOCO(1:14)//'.DIAG.TIME'
      CALL JEVEUO(DIAGI,'E',JDIAGI)
      CALL JEVEUO(DIAGT,'E',JDIAGT)
C
C ---
C  
      IF (ACCES.EQ.'I') THEN
        DO 10 I = 1,ZDIAG
          ZI(JDIAGI-1+I) = 0
          ZR(JDIAGT-1+I) = 0.D0
  10    CONTINUE                          
      ELSEIF (ACCES.EQ.'E') THEN
        IF (TYPOPE.EQ.'CONT') THEN
          ZI(JDIAGI-1+1) = ZI(JDIAGI-1+1)+VALI 
          ZI(JDIAGI-1+6) = VALI       
        ELSE IF (TYPOPE.EQ.'LIAC') THEN
          ZI(JDIAGI-1+2) = VALI
        ELSE IF (TYPOPE.EQ.'LIAF') THEN
          ZI(JDIAGI-1+3) = VALI                  
        ELSE IF (TYPOPE.EQ.'GEOM') THEN
          ZI(JDIAGI-1+4) = ZI(JDIAGI-1+4)+VALI   
        ELSE IF (TYPOPE.EQ.'FROT') THEN
          ZI(JDIAGI-1+5) = ZI(JDIAGI-1+5)+VALI    
          
        ELSE IF (TYPOPE.EQ.'TIMA') THEN
          ZR(JDIAGT-1+1) = ZR(JDIAGT-1+1)+VALR
        ELSE IF (TYPOPE.EQ.'TIMG') THEN
          ZR(JDIAGT-1+2) = ZR(JDIAGT-1+2)+VALR          
                                
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
      ELSE IF (ACCES.EQ.'L') THEN
        IF (TYPOPE.EQ.'CONT') THEN
          VALI = ZI(JDIAGI-1+6) 
        ELSE IF (TYPOPE.EQ.'LIAC') THEN
          VALI = ZI(JDIAGI-1+2) 
        ELSE IF (TYPOPE.EQ.'LIAF') THEN
          VALI = ZI(JDIAGI-1+3)          
        ELSE IF (TYPOPE.EQ.'GEOM') THEN
          VALI = ZI(JDIAGI-1+4)  
        ELSE IF (TYPOPE.EQ.'FROT') THEN
          VALI = ZI(JDIAGI-1+5)                      
        ELSE IF (TYPOPE.EQ.'CONC') THEN
          VALI = ZI(JDIAGI-1+1) 
                                               
        ELSE IF (TYPOPE.EQ.'TIMA') THEN
          VALR = ZR(JDIAGT-1+1)
        ELSE IF (TYPOPE.EQ.'TIMG') THEN
          VALR = ZR(JDIAGT-1+2)
                   
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF


      CALL JEDEMA()
      END
