      SUBROUTINE OP9999( ICOND , IER , IFIN )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            ICOND , IER , IFIN
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 13/01/2003   AUTEUR CIBHHLV L.VIVAN 
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
C     OPERATEUR DE CLOTURE
C     ------------------------------------------------------------------
C     FIN OP9999
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      IEND, IVERI, IFM
      CHARACTER*8  K8B, OUINON, OUIPER, TYPRES
      CHARACTER*16 FCHIER
      REAL*8       XTT, TEMPS(6)
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      IFIN = 1
      
C     TEST ERREUR E SANS ERREUR F      
      CALL FSIE()
      
      IF ( ICOND .NE. 0 ) GOTO 9999
C     -----  FIN DE LA ZONE DE TEST ------------------------------------
C
C     --- IMPRESSION DES SD RESULTAT
C
      IEND = 0
      CALL GETVTX(' ','FICHIER',1,1,1,FCHIER,L)
      IFM = IUNIFI( FCHIER )
      TYPRES = 'RESULTAT'
      CALL GCURES ( TYPRES, IEND, NBCMD, K8B )
      IF ( NBCMD .GT. 0 ) THEN
         IEND = 1
         CALL WKVECT ( '&&OP9999.NOM', 'V V K8', NBCMD, JCMD )
         CALL GCURES ( TYPRES, IEND, NBCMD, ZK8(JCMD) )
         DO 10 I = 1 , NBCMD
            WRITE(IFM,1000)
            CALL RSINFO ( ZK8(JCMD+I-1) , FCHIER )
 10      CONTINUE
         CALL JEDETR ( '&&OP9999.NOM' )
      ENDIF
C
C     --- IMPRESSION DES TEMPS PAR COMMANDE
      CALL GETVTX(' ','PERFORMANCE',1,1,1,OUIPER,L)
      IF(OUIPER .EQ. 'OUI') THEN
        IEND = 0
        CALL WRSTAT('RESULTAT',1,IEND)
      ENDIF
C
C     --- ETAT DE LA MEMOIRE ET DES REPERTOIRE JEVEUX ---
      CALL JEIMPM ( 'VIGILE','ETAT MEMOIRE DANS LA PROCEDURE "FIN"')
      CALL JEIMPR ( 'VIGILE',' ','REPERTOIRE DES BASES  DANS "FIN"')
C
C     --- IER = 0 ==> ON TERMINE NORMALEMENT ( ON L'ESPERE) ---
      IF ( IER .EQ. 0 ) CALL GCUOPR ( 1 , ICMD  )
      IUNERR = IUNIFI('ERREUR')
C
C     --- SUPPRESSION DES CONCEPTS TEMPORAIRES DES MACRO
      CALL JEDETC('G','.',1)
      CALL GETCMC(ICMD)
      CALL GCDETP(ICMD,'.')
C
C     --- RETASSAGE EVENTUEL DE LA GLOBALE
      CALL GETVTX(' ','RETASSAGE',1,1,1,OUINON,L)
      IF(OUINON .EQ. 'OUI') CALL JETASS('G')
C
C     --- CLOTURE DES FICHIERS ---
      CALL JELIBF( 'SAUVE' , 'G' )
      IF (IUNERR.GT.0) WRITE(IUNERR,*)
     +        '<I> <FIN> FERMETURE DE LA BASE "GLOBALE" EFFECTUEE.'
      CALL JELIBF( 'DETRUIT' , 'L' )
      CALL JELIBF( 'DETRUIT' , 'V' )
C
C     --- RETASSAGE EFFECTIF ----
      IF(OUINON .EQ. 'OUI') THEN
         CALL JXCOPY('G','GLOBALE','V','VOLATILE',NBEXT)
         IF (IUNERR.GT.0) WRITE(IUNERR,'(A,I2,A)')
     +   ' <I> <FIN> RETASSAGE DE LA BASE "GLOBALE" EFFECTUEE,', NBEXT,
     +   ' FICHIER(S) UTILISE(S).'
      ENDIF
C
C     --- IMPRESSION DES STATISTIQUES ( AVANT CLOTURE DE JEVEUX ) ---
      IF ( IER .EQ. 0 ) THEN
         IF (IUNERR.GT.0) WRITE(IUNERR,*)
     +        '<I> <FIN> ARRET NORMAL DANS "FIN" PAR APPEL A "JEFINI".'
      ELSE
         IF(IUNERR.GT.0) WRITE(IUNERR,*)
     +        '<I>   ARRET DANS "FIN", DES ERREURS AYANT ETE DETECTEES'
      ENDIF
C
C
C     --- INFORMATION SUR LE TEMPS GLOBAL ---
      CALL UTTCPU(-10,'FIN',6,TEMPS)
      CALL UTDEBM('I','INFORMATION TEMPS D''EXECUTION','(EN SECONDE)')

      CALL UTIMPR('L','     TEMPS CPU TOTAL ..............',1,TEMPS(3))

      CALL UTIMPR('L','     TEMPS CPU USER TOTAL .........',1,TEMPS(5))
      CALL UTIMPR('L','     TEMPS CPU SYSTEME TOTAL ......',1,TEMPS(6))
      CALL UTIMPR('L','     TEMPS CPU RESTANT ............',1,TEMPS(1))
      CALL UTFINM()
C
      IF(OUIPER .EQ. 'NON') THEN
C     --- IMPRESSION DES STATISTIQUES DE L'OPERATEUR ---
         ISTAT = 2
         CALL EXSTAT ( ISTAT ,ICOND , XTT )
      ELSE
C
C     --- IMPRESSION DANS LE FICHIER MESSAGE ---
C     --- DES STATISTIQUES DU JOB ---
         ISTAT = 3
         CALL EXSTAT ( ISTAT ,ICOND , XTT )
         IFR = IUNIFI('RESULTAT')
         WRITE(IFR,'(1X,''*'',1X,A,3(1X,'':'',1X,F10.2),1X,''*'')')
     +        'TOTAL_JOB       ',
     +        TEMPS(5), TEMPS(6), TEMPS(3)
         WRITE(IFR,'(1X,59(''*''))')
      ENDIF
C
C     --- LA CLOTURE DE JEVEUX ---
C
      CALL JEFINI ( 'NORMAL' )
C
 1000 FORMAT(/,1X,'======>')
C
 9999 CONTINUE
      CALL JEDEMA()
      END
