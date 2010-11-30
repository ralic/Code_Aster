      SUBROUTINE MMIMP1(IFM   ,NOMA  ,DEFICO,RESOCO)
C      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF DEBUG  DATE 14/09/2010   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER      IFM
      CHARACTER*8  NOMA
      CHARACTER*24 DEFICO,RESOCO
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE - IMPRESSIONS)
C
C AFFICHAGE APPARIEMENT
C      
C ----------------------------------------------------------------------
C
C
C IN  IFM    : UNITE D'IMPRESSION DU MESSAGE
C IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
C IN  RESOCO : SD POUR LA RESOLUTION DU CONTACT
C IN  NOMA   : NOM DU MAILLAGE
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      CHARACTER*32 JEXNUM,JEXATR
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER      CFMMVD,ZTABF
      INTEGER      POSMAE,NUMMAE,NUMMAM,NUMNOE,JDECME
      CHARACTER*8  NOMMAE,NOMMAM,NOMNOE 
      REAL*8       KSIPC1,KSIPC2,KSIPR1,KSIPR2,WPC,R8BID,SEUILI
      INTEGER      XA,XS,TYPBAR,TYPRAC
      REAL*8       TAU1(3),TAU2(3),NORM(3)
      CHARACTER*24 TABFIN
      INTEGER      JTABF
      INTEGER      CFDISI,MMINFI
      INTEGER      IPTM,IZONE,IMAE,INOE,IPTC
      INTEGER      NDIMG,NZOCO,NNOE,NPTM,NBMAE
      INTEGER      IACNX1,ILCNX1
      LOGICAL      MMINFL,LVERI
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()   
      WRITE (IFM,*) '<CONTACT> ... RESULTAT DE L''APPARIEMENT' 
C
C --- INITIALISATIONS
C
      NDIMG  = CFDISI(DEFICO,'NDIM' )   
      NZOCO  = CFDISI(DEFICO,'NZOCO')    
C      
C --- RECUPERATION DE QUELQUES DONNEES      
C
      TABFIN = RESOCO(1:14)//'.TABFIN'
      CALL JEVEUO(TABFIN,'L',JTABF)
      ZTABF  = CFMMVD('ZTABF')
C
C --- ACCES MAILLAGE
C      
      CALL JEVEUO(NOMA//'.CONNEX','L',IACNX1)
      CALL JEVEUO(JEXATR(NOMA//'.CONNEX','LONCUM'),'L',ILCNX1) 
C
C --- BOUCLE SUR LES ZONES
C
      IPTC   = 1     
      DO 10 IZONE = 1,NZOCO
C 
C ----- MODE VERIF: ON SAUTE LES POINTS
C  
        LVERI  = MMINFL(DEFICO,'VERIF' ,IZONE )
        IF (LVERI) THEN
          GOTO 25
        ENDIF            
C
C ----- INFORMATION SUR LA ZONE 
C         
        JDECME = MMINFI(DEFICO,'JDECME',IZONE )
        NBMAE  = MMINFI(DEFICO,'NBMAE' ,IZONE )      
C    
C ----- BOUCLE SUR LES MAILLES ESCLAVES
C
        DO 20 IMAE = 1,NBMAE
          POSMAE = JDECME + IMAE
C
C ------- NOMBRE DE POINTS DE CONTACT
C          
          CALL MMINFM(POSMAE,DEFICO,'NPTM',NPTM  )
C
C ------- REPERAGE MAILLE ESCLAVE
C
          NUMMAE = NINT(ZR(JTABF+ZTABF*(IPTC-1)+1))
          CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUMMAE),NOMMAE)
          NNOE   = ZI(ILCNX1+NUMMAE) - ZI(ILCNX1-1+NUMMAE)
C
C ------- INFOS SUR MAILLE ESCLAVE
C        
          WRITE (IFM,1000) NOMMAE,IZONE,NNOE,NPTM      
 1000     FORMAT (' <CONTACT>     * MAILLE ESCLAVE ',A8,' ( ZONE ',
     &           I5,') - (',
     &           I5,' NOEUDS ) - (',
     &           I5,' POINTS DE CONTACT )' )
          DO 21 INOE = 1,NNOE
            NUMNOE = ZI(IACNX1+ZI(ILCNX1-1+NUMMAE)-2+INOE)
            CALL JENUNO(JEXNUM(NOMA//'.NOMNOE',NUMNOE),NOMNOE)
            WRITE (IFM,1001) NOMNOE
  21      CONTINUE        
 1001     FORMAT (' <CONTACT>        NOEUD :',A8)
C
C ------- BOUCLE SUR LES POINTS
C      
          DO 30 IPTM = 1,NPTM      
C
C --------- POINT DE CONTACT EN COURS
C     
            WRITE(IFM,2000) IPTM
            KSIPC1 = ZR(JTABF+ZTABF*(IPTC-1)+3)       
            KSIPC2 = ZR(JTABF+ZTABF*(IPTC-1)+4)      
            WPC    = ZR(JTABF+ZTABF*(IPTC-1)+14) 
            WRITE(IFM,3000) KSIPC1,KSIPC2,WPC           
 2000 FORMAT (' <CONTACT>     ** POINT DE CONTACT ',I3)
 3000 FORMAT (' <CONTACT>        SITUE EN  : <',
     &         E10.3,',',E10.3,'> - POIDS INTEGRATION: ',E10.3)     
C
C --------- REPERAGE MAILLE MAITRE
C
            NUMMAM = NINT(ZR(JTABF+ZTABF*(IPTC-1)+2))
            CALL JENUNO(JEXNUM(NOMA//'.NOMMAI',NUMMAM),NOMMAM)       
C
C --------- ETAT DU NOEUD
C
            KSIPR1 = ZR(JTABF+ZTABF*(IPTC-1)+5)
            KSIPR2 = ZR(JTABF+ZTABF*(IPTC-1)+6)
            IF (ZR(JTABF+ZTABF*(IPTC-1)+18).EQ.1.D0) THEN
              WRITE(IFM,*) '<CONTACT>        EXCLUS CONTACT    '
            ELSEIF (ZR(JTABF+ZTABF*(IPTC-1)+19).GE.1.D0) THEN
              WRITE(IFM,*) '<CONTACT>        EXCLUS FROTTEMENT '
            ELSE
              WRITE(IFM,2003) NOMMAM,KSIPR1,KSIPR2             
            ENDIF  
 2003 FORMAT (' <CONTACT>        SE PROJETTE SUR LA MAILLE MAITRE ',
     &        A8,' EN  <',E10.3,',',E10.3,'>')
     
            TYPBAR  = NINT(ZR(JTABF+ZTABF*(IPTC-1)+20))
            TYPRAC  = NINT(ZR(JTABF+ZTABF*(IPTC-1)+21))
          
            IF (TYPBAR.NE.0) THEN
              WRITE(IFM,5000) TYPBAR
            ENDIF
            IF (TYPRAC.NE.0) THEN
              WRITE(IFM,5001) TYPRAC
            ENDIF          
 5000 FORMAT (' <CONTACT>        MAILLE EN FOND_FISSURE  : <',
     &        I5,'>')
 5001 FORMAT (' <CONTACT>        MAILLE POUR RACCORD     : <',
     &        I5,'>')              
C
C --------- REPERE LOCAL
C      
            TAU1(1) = ZR(JTABF+ZTABF*(IPTC-1)+7 )
            TAU1(2) = ZR(JTABF+ZTABF*(IPTC-1)+8 )
            TAU1(3) = ZR(JTABF+ZTABF*(IPTC-1)+9 )
            TAU2(1) = ZR(JTABF+ZTABF*(IPTC-1)+10)
            TAU2(2) = ZR(JTABF+ZTABF*(IPTC-1)+11)
            TAU2(3) = ZR(JTABF+ZTABF*(IPTC-1)+12)
            WRITE(IFM,2002) TAU1(1),TAU1(2),TAU1(3),
     &                    TAU2(1),TAU2(2),TAU2(3)           
 2002 FORMAT (' <CONTACT>        TANGENTES : <',
     &         E10.3,',',E10.3,',',E10.3,'> <',
     &         E10.3,',',E10.3,',',E10.3,'>')  
      
            CALL MMNORM(NDIMG,TAU1,TAU2,NORM,R8BID)
            WRITE(IFM,2001) NORM(1),NORM(2),NORM(3)
          
 2001 FORMAT (' <CONTACT>        NORMALE   : <',
     &         E10.3,',',E10.3,',',E10.3,'>')  
C
C --------- ETAT DE CONTACT
C
            XS     = NINT(ZR(JTABF+ZTABF*(IPTC-1)+22))
            XA     = NINT(ZR(JTABF+ZTABF*(IPTC-1)+23))
            IF (XS.EQ.0) THEN
              IF (XA.EQ.0) THEN
                WRITE(IFM,7000)
              ELSEIF (XA.EQ.1) THEN
                WRITE(IFM,7001)
              ELSE
                CALL ASSERT(.FALSE.)
              ENDIF          
            ELSEIF (XS.EQ.1) THEN
              IF (XA.EQ.1) THEN
                WRITE(IFM,7002)
              ENDIF
            ELSE
              CALL ASSERT(.FALSE.)
            ENDIF
 7000 FORMAT (' <CONTACT>        ETAT : EN CONTACT') 
 7001 FORMAT (' <CONTACT>        ETAT : PAS EN CONTACT')
 7002 FORMAT (' <CONTACT>        ETAT : EN CONTACT ')     
C
C --------- AUTRES INFOS
C     
            SEUILI = ZR(JTABF+ZTABF*(IPTC-1)+16)
            WRITE(IFM,4002) SEUILI
 4002 FORMAT (' <CONTACT>        SEUIL_INIT : <',E10.3,'>')      
C
C --------- LIAISON SUIVANTE
C
            IPTC   = IPTC + 1             
            
  30      CONTINUE
  20    CONTINUE
  25    CONTINUE
  10  CONTINUE
C
      CALL JEDEMA()
           
      END
