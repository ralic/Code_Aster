      SUBROUTINE OP0134 ( IER )
      IMPLICIT   NONE
      INTEGER             IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 20/09/2004   AUTEUR DURAND C.DURAND 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C     CALCUL D'UNE FONCTION INTERPRETEE
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      IFM, NIV, INUMEX, N1, LNOVA, NBNOVA, ITYPFO, NBVALR,
     +             NBVAL, LVAL, NBVAL2, LVALE, LFON, IVAL,JPFI,
     +             LFITA, N2, NBVALF, LPARA, LVALF,LONT, I, JVALS,N3
      REAL*8       RVAL
      LOGICAL      COMPL
      CHARACTER*2  PROLGD,PROGDF
      CHARACTER*4  INTERP(2),INTERF(2)
      CHARACTER*8  K8B, STATUT, NOMRES, NOMPAR,NOMPAF
      CHARACTER*16 NOMCMD, TYPRES
      CHARACTER*19 NOMFON, NOMFIN, LISTR, LISTF, TYPCO
      CHARACTER*32 JEXNUM
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      IER = 0
      CALL INFMAJ
      CALL INFNIV ( IFM, NIV )
C
      CALL GETCMD ( NOMFON, TYPRES, NOMCMD, STATUT, INUMEX )
C
      CALL GETVID ( ' ', 'FONCTION', 1,1,1, NOMFIN, N1 )
      CALL GETTCO ( NOMFIN, TYPCO )
C
      IF ( STATUT .EQ. 'MODIFIE') THEN
         IF ( NOMFON .NE. NOMFIN ) THEN
            CALL UTDEBM('F',NOMCMD,'ON NE PEUT UTILISER UNE FONCTION'//
     +                      ' DEJA EXISTANTE DIFFERENTE EN ENTREE ET'//
     +                      ' EN SORTIE.')
            CALL UTIMPK('L','FONCTION EN ENTREE ',1,NOMFIN)
            CALL UTIMPK('L','FONCTION EN SORTIE ',1,NOMFON)
            CALL UTFINM()
         ENDIF
      ENDIF
C
C --- RECUPERATIONS DES MOTS CLES
C
      CALL GETVTX ( ' ', 'NOM_PARA'   , 1,1,1, NOMPAR     , N1 ) 
      CALL GETVTX ( ' ', 'PROL_GAUCHE', 1,1,1, PROLGD(1:1), N1 )
      CALL GETVTX ( ' ', 'PROL_DROITE', 1,1,1, PROLGD(2:2), N1 )
      CALL GETVTX ( ' ', 'NOM_RESU'   , 1,1,1, NOMRES     , N1 )
      CALL GETVTX ( ' ', 'INTERPOL'   , 1,1,2, INTERP     , N1 )
      IF ( N1 .EQ. 1 ) INTERP(2) = INTERP(1)
C 
      CALL GETVTX ( ' ', 'NOM_PARA_FONC'   , 1,1,1, NOMPAF    , N1 )
      CALL GETVTX ( ' ', 'PROL_GAUCHE_FONC', 1,1,1, PROGDF(1:1), N1 )
      CALL GETVTX ( ' ', 'PROL_DROITE_FONC', 1,1,1, PROGDF(2:2), N1 )
      CALL GETVTX ( ' ', 'INTERPOL_FONC'   , 1,1,2, INTERF     , N1 )
      IF ( N1 .EQ. 1 ) INTERF(2) = INTERF(1)
C       
C --- ACCES AU PARAMETRE POUR LES NAPPES ET A LA VARIABLE
C     POUR LES FONCTIONS      
C
      CALL GETVR8 ( ' ', 'VALE_PARA', 1,1,0, RVAL, NBVALR )
      IF ( NBVALR .NE. 0 ) THEN 
         NBVAL  = - NBVALR
         CALL WKVECT ( '&&OP0134.VALE', 'V V R', NBVAL, LVAL )
         CALL GETVR8 ( ' ', 'VALE_PARA', 1,1,NBVAL, ZR(LVAL), N1 )
      ELSE 
         CALL GETVID ( ' ', 'LIST_PARA', 1,1,1, LISTR, N1 )
         CALL JEVEUO ( LISTR//'.VALE', 'L', LVAL)
         CALL JELIRA ( LISTR//'.VALE', 'LONUTI', NBVAL, K8B )
      ENDIF
C      
C --- NAPPE OU FONCTION
C
      IF (TYPCO(1:7).EQ.'FORMULE') THEN
C                 
C ---    FORMULE REELLE
C ---    FORMULE (( COMPLEXE ))
C
         COMPL = .FALSE.
         CALL JEVEUO ( NOMFIN//'.NOVA', 'L', LNOVA )
         CALL JELIRA ( NOMFIN//'.NOVA', 'LONUTI', NBNOVA, K8B )
         CALL WKVECT ( '&&OP0134.NOVA', 'V V K8', NBNOVA, LNOVA )
      ELSEIF (TYPCO(1:8).EQ.'FONCTION') THEN
         NBNOVA=1
         CALL WKVECT ( '&&OP0134.NOVA', 'V V K8', NBNOVA, LNOVA )
         IF (TYPCO(1:10).EQ.'FONCTION_C') THEN
            COMPL = .TRUE.
         ELSE
            COMPL = .FALSE.
         ENDIF
      ELSEIF (TYPCO(1:5).EQ.'NAPPE') THEN
         NBNOVA=2
         CALL WKVECT ( '&&OP0134.NOVA', 'V V K8', NBNOVA, LNOVA )
         COMPL = .FALSE.
      ENDIF
C
      IF (NBNOVA.EQ.1) THEN
C ------------------------------------------------------------------    
C                 FONCTION  
C ------------------------------------------------------------------   
         ZK8(LNOVA)=NOMPAR
         CALL CALCFO(COMPL,NBVAL,NOMFON,LVAL,NOMFIN,STATUT,
     +               INTERP,LNOVA,NOMRES,PROLGD,NOMCMD,NBNOVA )
C
      ELSE IF (NBNOVA.EQ.2) THEN
C ------------------------------------------------------------------    
C                 NAPPE  
C ------------------------------------------------------------------ 

         ZK8(LNOVA)=NOMPAF
         ZK8(LNOVA+1)=NOMPAR
C
C --- ACCES A L'ABSCISSE DES FONCTIONS DE LA NAPPE
C         
         CALL GETVID ( ' ', 'LIST_PARA_FONC', 1,1,1, LISTF, N1 )
         IF (N1.EQ.0) THEN
            CALL GETVR8 ( ' ', 'VALE_PARA_FONC', 1,1,0, RVAL, NBVALF )
            NBVALF=-NBVALF
            CALL WKVECT ( '&&OP0134.VALF', 'V V R', NBVALF, LVALF )
            CALL GETVR8 ( ' ', 'VALE_PARA_FONC', 1,1,NBVALF, 
     +                    ZR(LVALF),N3 )
         ELSE
             CALL JEVEUO ( LISTF//'.VALE', 'L', LVALF)
            CALL JELIRA ( LISTF//'.VALE', 'LONUTI', NBVALF, K8B )
         ENDIF
C
         CALL CALCNA( COMPL,NBVAL,NBVALF,NOMFON,LVAL,NOMFIN,
     +                   STATUT,INTERF,INTERP,LNOVA,PROGDF,PROLGD,
     +                   LVALF,NOMCMD,NOMRES,NBNOVA )
C         
      ELSE
C      
         CALL UTMESS('F',NOMCMD,'FONCTION A UNE OU DEUX VARIABLES 
     +                                 ADMISE')
C     
      ENDIF
C
C     --- VERIFICATION QU'ON A BIEN CREER UNE FONCTION ---
C         ET REMISE DES ABSCISSES EN ORDRE CROISSANT
      CALL ORDONN(NOMFON,NOMCMD,0)
C
      CALL TITRE
      IF (NIV.GT.1) CALL FOIMPR (NOMFON,NIV,IFM,0,LISTR)      
C
      CALL JEDETR('&&OP0134.NOVA') 
      CALL JEDETR('&&OP0134.VALE') 
      CALL JEDETR('&&OP0134.VALF') 
C        
      CALL JEDEMA()
      END
