      SUBROUTINE JEFINI ( COND )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 22/02/2011   AUTEUR LEFEBVRE J-P.LEFEBVRE 
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_12 CRS_505
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)       COND
C     ==================================================================
      PARAMETER  ( N = 5 )
C
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      INTEGER          NBFIC
      COMMON /IPARJE/  NBFIC
C     ------------------------------------------------------------------
      CHARACTER*2      DN2
      CHARACTER*5      CLASSE
      CHARACTER*8                  NOMFIC    , KSTOUT    , KSTINI
      COMMON /KFICJE/  CLASSE    , NOMFIC(N) , KSTOUT(N) , KSTINI(N) ,
     &                 DN2(N)
C
      INTEGER          IPGC,KDESMA(2),LGD,LGDUTI,KPOSMA(2),LGP,LGPUTI
      COMMON /IADMJE/  IPGC,KDESMA,   LGD,LGDUTI,KPOSMA,   LGP,LGPUTI
      INTEGER          LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOUA , LOR8 , LOC8
      INTEGER          LDYN , LGDYN , NBDYN , NBFREE
      COMMON /IDYNJE/  LDYN , LGDYN , NBDYN , NBFREE
      INTEGER          ICDYN , MXLTOT
      COMMON /XDYNJE/  ICDYN , MXLTOT
      REAL *8          MXDYN , MCDYN , MLDYN , VMXDYN , LGIO  
      COMMON /RDYNJE/  MXDYN , MCDYN , MLDYN , VMXDYN , LGIO(2)
C     ==================================================================
      INTEGER          VALI(7) , INFO, IFM, IRES
      CHARACTER*8      KCOND , STAOU
      CHARACTER*24     LADATE
      REAL*8           RVAL(9)
C     ------------------------------------------------------------------
C
      KCOND  = COND ( 1: MIN( LEN(COND) , LEN(KCOND) ) )
      CALL ASSERT (     KCOND .EQ. 'NORMAL  ' .OR. KCOND .EQ. 'ERREUR  '
     &             .OR. KCOND .NE. 'TEST    '  ) 
      IF ( KCOND .EQ. 'NORMAL  ' .OR. KCOND .EQ. 'TEST    ' ) THEN
         STAOU = '        '
      ELSE
         STAOU = 'SAUVE   '
      ENDIF
C     -------------  EDITION DES REPERTOIRES ---------------------------
      IF ( KCOND .EQ. 'TEST    '  ) THEN
        DO 5 I = 1 , NBFIC
          IF ( CLASSE(I:I) .NE. ' ' ) THEN
            CALL JEIMPR ( 'MESSAGE' , CLASSE(I:I) ,
     &                    '     JEFINI     ' // KCOND )
          ENDIF
   5    CONTINUE
C     -------------  EDITION SEGMENTATION MEMOIRE ----------------------
        CALL JEIMPM ( 'MESSAGE' , '     JEFINI     ' // KCOND   )
      ENDIF
C     -------------  LIBERATION FICHIER --------------------------------
      IF ( KCOND .NE. 'ERREUR  ' )   THEN
        INFO = 1
        DO 10 I = 1 , NBFIC
          IF ( CLASSE(I:I) .NE. ' ' ) THEN
            CALL JELIBF ( STAOU , CLASSE(I:I) , INFO )
          ENDIF
   10   CONTINUE
C       -----------  DESALLOCATION GESTION DES MARQUES -----------------
        IF ( KDESMA(2) .NE. 0) THEN
          CALL JJLIDY (KDESMA(2), KDESMA(1))
        ELSE IF (KDESMA(1) .NE. 0) THEN
          CALL JJLIBP (KDESMA(1))
        ENDIF  
        IF ( KPOSMA(2) .NE. 0) THEN
          CALL JJLIDY (KPOSMA(2), KPOSMA(1))
        ELSE IF (KPOSMA(1) .NE. 0) THEN
          CALL JJLIBP (KPOSMA(1))
        ENDIF  
        KDESMA(1) = 0
        KDESMA(2) = 0
        KPOSMA(1) = 0
        KPOSMA(2) = 0
C       -----------  DESALLOCATION MEMOIRE -----------------------------
        CALL JXLIBM ( ISZON , LISZON )
C
      ELSE
        CALL ASSERT (.FALSE.)
      ENDIF
C
C     --- IMPRESSION DES CONSOMMATIONS MEMOIRE ---
      CALL JEINFO(RVAL)
      IFM = IUNIFI('MESSAGE')
      IRES = IUNIFI('RESULTAT')
C
      IF(IRES .GT. 0) THEN
        WRITE(IRES,*) ' '
        WRITE(IRES,'(2A,F11.2,A)') 
     +        ' <I> <FIN> MEMOIRE JEVEUX MINIMALE REQUISE POUR ',
     +        'L''EXECUTION :                ',RVAL(2),' Mo'
        IF (RVAL(9).GT.0) THEN 
          WRITE(IRES,'(2A,F11.2,A)') 
     +        ' <I> <FIN> MAXIMUM DE MEMOIRE UTILISEE PAR LE PROCESSUS'
     +        ,' LORS DE L''EXECUTION :',RVAL(9)/1024,' Mo'
        ENDIF
      ENDIF
C
      IF ( KCOND .NE. 'TEST    ') THEN
        IF (IFM .GT. 0) THEN
          WRITE(IFM,*) ' '
          WRITE(IFM,*) '<I>       FERMETURE DES BASES EFFECTUEE'
          IF ( LDYN .EQ. 1 ) THEN
            VALI(1) = NINT(MXDYN/(1024*1024))
            VALI(2) = LISZON*LOIS/(1024*1024)
            VALI(3) = NBDYN
            VALI(4) = NBFREE
            VALI(5) = NINT(MLDYN/(1024*1024))
            VALI(6) = NINT(LGIO(1)/(1024*1024))
            VALI(7) = NINT(LGIO(2)/(1024*1024))
            WRITE(IFM,*) ' '
            WRITE(IFM,*) '  STATISTIQUES CONCERNANT L'''
     &                 //'ALLOCATION DYNAMIQUE :' 
            WRITE(IFM,*) '    TAILLE CUMULEE MAXIMUM            :',
     &                 VALI(1),' Mo.' 
            WRITE(IFM,*) '    TAILLE CUMULEE LIBEREE            :', 
     &                 VALI(5),' Mo.' 
            WRITE(IFM,*) '    NOMBRE TOTAL D''ALLOCATIONS        :',    
     &                 VALI(3) 
            WRITE(IFM,*) '    NOMBRE TOTAL DE LIBERATIONS       :',    
     &                 VALI(4) 
            WRITE(IFM,*) '    APPELS AU MECANISME DE LIBERATION :',
     &                 ICDYN 
            WRITE(IFM,*) '    TAILLE MEMOIRE CUMULEE RECUPEREE  :',    
     &                 MXLTOT,' Mo.' 
            WRITE(IFM,*) '    VOLUME DES LECTURES               :',
     &                  VALI(6),' Mo.'    
            WRITE(IFM,*) '    VOLUME DES ECRITURES              :',
     &                  VALI(7),' Mo.'    
            WRITE(IFM,*) ' '
          ENDIF         
          WRITE(IFM,*) '  MAXIMUM DE MEMOIRE UTILISEE PAR'
          WRITE(IFM,'(2A,F11.2,A)') '   LE PROCESSUS LORS DE '
     &                 ,'L''EXECUTION    :  ',RVAL(9)/1024,' Mo.'
          WRITE(IFM,*) ' '
          CALL ENLIRD(LADATE)
          WRITE(IFM,*) '<I>       FIN D''EXECUTION LE : '//LADATE
C
        ENDIF
C
C       --- ON FERME TOUT ---
        CALL ULCLOS
C
        CALL XFINI(19)
      ENDIF
      END
