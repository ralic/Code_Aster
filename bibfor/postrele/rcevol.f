      SUBROUTINE RCEVOL ( TYPTAB, NOMMAT, NBOPT, OPTION )
      IMPLICIT   NONE
      INTEGER             NBOPT
      CHARACTER*8         NOMMAT
      CHARACTER*16        TYPTAB, OPTION(*)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 06/04/2004   AUTEUR DURAND C.DURAND 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C
C     OPERATEUR POST_RCCM:    TYPE_ANALYSE = 'COMPOSANT'
C                           TYPE_RESU_MECA = 'EVOLUTION'
C
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      NBFS, NBFT, J, N1, IOCC, 
     +             NPARA, NBPAR, NBORDM, NBORDT, NBORDR
      REAL*8       VALRES(3), PARA(3), SM, EREFE, E,  RBID, R8VIDE
      LOGICAL      RCCMPM, RCCMSN, SNTHER, FATIZH, LNOEU, LINTI
      CHARACTER*2  CODRET(3)
      CHARACTER*5  KOCC
      CHARACTER*8  K8B, NOMRES, NOMPAR, NOMVAL(3), NOMA,
     +             TYPCO, COURBE,  TYPARA(36), PARASG
      CHARACTER*16 NOMCMD, CONCEP, PHENOM, NOMCHA, NOMCH1, 
     +             NOPARA(36), NCHEFF, NCHEFT, INTITU
      CHARACTER*24 XNOMCP, XNUMCP, XNOMCT, XNUMCT, LSTNAC
C DEB ------------------------------------------------------------------
C
      CALL GETRES ( NOMRES , CONCEP , NOMCMD )
C
      CALL GETFAC ( 'SEGMENT'     , NBFS )
      CALL GETFAC ( 'TRANSITOIRE' , NBFT )
C
      CALL GETVTX ( 'TRANSITOIRE', 'NOM_CHAM',1,1,1, NOMCHA, N1 )
      DO 10, IOCC = 2, NBFT, 1
         CALL GETVTX ( 'TRANSITOIRE', 'NOM_CHAM',IOCC,1,1,NOMCH1,N1)
         IF ( NOMCHA .NE. NOMCH1 )  THEN
            CALL CODENT ( IOCC, 'G', KOCC )
            CALL UTMESS('F',NOMCMD,' LES NOM_CHAM '//NOMCHA//
     +        ' ET '//NOMCH1//' DONNES POUR LES OCCURENCES 1 ET '//
     +        KOCC//' DU MOT-FACTEUR "TRANSITOIRE" SONT DIFFERENTS.')
         ENDIF
 10   CONTINUE
C
      LNOEU = .FALSE.
      DO 20, IOCC = 1, NBFS, 1
         CALL GETVID ( 'SEGMENT', 'GROUP_NO', IOCC,1,0, K8B, N1 )
         IF ( N1 .NE. 0 )  LNOEU = .TRUE.
         CALL GETVID ( 'SEGMENT', 'NOEUD'   , IOCC,1,0, K8B, N1 )
         IF ( N1 .NE. 0 )  LNOEU = .TRUE.
 20   CONTINUE
      IF ( LNOEU )  THEN
         CALL GETVID ( ' ', 'MAILLAGE', 1,1,1, NOMA, N1 )
         IF ( N1 .EQ. 0 )  THEN
            CALL UTMESS('F',NOMCMD,' LE MAILLAGE EST OBLIGATOIRE '//
     +          'SI LES MOTS CLES GROUP_NO OU NOEUD SONT PRESENTS.')
         ENDIF
      ENDIF
C
C     ------------------------------------------------------------------
C                             LA TABLE 
C     ------------------------------------------------------------------
      CALL PRCCM0 ( NBOPT, OPTION, NBFT, NPARA, NOPARA, TYPARA,  
     +              RCCMPM, RCCMSN, SNTHER, FATIZH, TYPTAB )
C
      CALL TBCRSD ( NOMRES, 'G' )
      CALL TBAJPA ( NOMRES, NPARA, NOPARA, TYPARA )
C
C     ------------------------------------------------------------------
C                            LE MATERIAU
C     ------------------------------------------------------------------
C
      NBPAR     = 0
      NOMPAR    = ' '
      NOMVAL(1) = 'SM'
      CALL RCVALE ( NOMMAT, 'RCCM', NBPAR, NOMPAR, RBID, 1,
     +              NOMVAL, SM, CODRET, 'F ' )
C
      PARA(1) = R8VIDE()
      PARA(2) = R8VIDE()
      PARA(3) = R8VIDE()
      IF ( FATIZH ) THEN
         CALL RCCOME ( NOMMAT, 'FATIGUE', PHENOM, CODRET )
         IF ( CODRET(1) .EQ. 'NO' ) THEN
            CALL UTMESS('F',NOMCMD,'IL FAUT DEFINIR LE '//
     +                   'COMPORTEMENT "FATIGUE" DANS DEFI_MATERIAU')
         ENDIF
         CALL RCCOME ( NOMMAT, 'ELAS', PHENOM, CODRET )
         IF ( CODRET(1) .EQ. 'NO' ) THEN
            CALL UTMESS('F',NOMCMD,'IL FAUT DEFINIR LE '//
     +                   'COMPORTEMENT "ELAS" DANS DEFI_MATERIAU')
         ENDIF
C
         NOMVAL(1) = 'M_KE'
         NOMVAL(2) = 'N_KE'
         CALL RCVALE ( NOMMAT, 'RCCM', NBPAR, NOMPAR, RBID, 2, 
     +                 NOMVAL, VALRES, CODRET, 'F ' )
         PARA(1) = VALRES(1)
         PARA(2) = VALRES(2)
C
         NOMVAL(1) = 'E_REFE'
         CALL RCVALE ( NOMMAT, 'FATIGUE', NBPAR, NOMPAR, RBID, 1, 
     +                 NOMVAL, EREFE, CODRET, 'F ' )
C
         NOMVAL(1) = 'E'
         CALL RCVALE ( NOMMAT, 'ELAS', NBPAR, NOMPAR, RBID, 1, 
     +                 NOMVAL, E, CODRET, 'F ' )
         PARA(3) = EREFE / E
      ENDIF
C
C     ------------------------------------------------------------------
C                        TRAITEMENT DES CHAMPS 
C     ------------------------------------------------------------------
C
      XNOMCP = '&&OP0165.NOM.COMPOSANTES'
      XNUMCP = '&&OP0165.NUM.COMPOSANTES'
      NCHEFF = '&&OP0165.CHAMP19'
      CALL PRGARG ( 'TRANSITOIRE', 'RESULTAT', XNOMCP, XNUMCP, NCHEFF,
     +               NBORDM, RCCMPM, RCCMSN, SNTHER )
C
      XNOMCT = '&&OP0165.NOM.COMPOSANT_T'
      XNUMCT = '&&OP0165.NUM.COMPOSANT_T'
      NCHEFT = '&&OP0165.CHAMP_T'
      IF ( SNTHER ) THEN
         CALL PRGARG ( 'TRANSITOIRE', 'RESU_SIGM_THER', XNOMCT, XNUMCT,
     +              NCHEFT, NBORDT, .FALSE., .FALSE., .FALSE. )
      ENDIF
C
C     ------------------------------------------------------------------
C              TRAITEMENT DES OPTIONS SUIVANT LES CHEMINS
C     ------------------------------------------------------------------
C
      LSTNAC = '&&OP0165.MES_NOEUDS'
      NOMA   = ' ' 
      CALL GETVID ( ' ', 'MAILLAGE', 1,1,1, NOMA, N1 )
C
      DO 30 J = 1 , NBFS
C
         CALL GETVID ( 'SEGMENT', 'CHEMIN'  ,J,1,1, COURBE, N1 )
         IF ( N1 .EQ. 0 ) THEN
            TYPCO  = 'NOEUDS'
            CALL PRGNOE ( COURBE, PARASG, NOMA, 'SEGMENT', J, LSTNAC )
         ELSE
            TYPCO  = 'CHEMIN'
            PARASG = 'CHEMIN'
         ENDIF
C
         LINTI = .FALSE.
         INTITU = COURBE
         CALL GETVTX ( 'SEGMENT','INTITULE' , J,1,1, INTITU, N1 )
         IF ( N1 .NE. 0 )  LINTI = .TRUE.
C
         IF ( FATIZH ) THEN
C
            CALL PRCCM1 ( 'TRANSITOIRE', NBFT, TYPCO, COURBE, SM, 
     +           NBORDM, NBORDR, XNOMCP, XNUMCP, NCHEFF, NOMA, LSTNAC )
C
            CALL PRCCM2 ( NOMMAT, NBORDR, PARA, SM )
C
         ENDIF
C
         IF ( RCCMPM .OR. RCCMSN ) THEN
C
            CALL PRCCM7 ( 'TRANSITOIRE', NBFT, TYPCO, COURBE, SM,  
     +                    XNOMCP, XNUMCP, NCHEFF, RCCMPM, RCCMSN,  
     +                    SNTHER, NCHEFT, NOMA, LSTNAC )
C
         ENDIF
C
         IF ( TYPTAB .EQ. 'VALE_MAX' ) THEN
C
            CALL PRCCM6 ( NOMRES, NBFT, NBORDR, PARASG, COURBE, LINTI,
     +                    INTITU, SM, NCHEFF, RCCMPM,
     +                    RCCMSN, SNTHER, FATIZH )
         ELSE
C
            CALL PRCCM5 ( NOMRES, NBFT, NBORDR, PARASG, COURBE, LINTI,
     +                    INTITU, SM, NCHEFF, RCCMPM, 
     +                    RCCMSN, SNTHER, FATIZH )
C
         ENDIF
C
         CALL JEDETR ( LSTNAC )
C
 30   CONTINUE
C
      END
