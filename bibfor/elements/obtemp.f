      SUBROUTINE OBTEMP(SDFET1)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 16/06/2004   AUTEUR DURAND C.DURAND 
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
C TOLE CRP_20
C-----------------------------------------------------------------------
C    - FONCTION REALISEE:  CREATION DE LA STRUCTURE DE DONNEES FETI.
C      ELLE EST CONTENUE DANS UN CONCEPT PRODUIT DE TYPE SD_FETI
C   ----------
C   ATTENTION
C   ROUTINE TEMPORAIRE POUR LE CAS-TEST FETI. A DETRUIRE DES QUE LE
C   PARTITIONNEMENT VIA DEFI_PART_FETI EST EN PLACE.
C   ----------
C
C IN SDFETI   : NOM DU CONCEPT PRODUIT
C OUT SDFETI  : LE CONCEPT EST CREE ET INSTANCIE
C
C   -------------------------------------------------------------------
C     SUBROUTINES APPELLEES:
C       MESSAGE:INFNIV.
C       JEVEUX:JEMARQ,JEDEMA,JECROC,JEECRA,JEVEUO,WKVECT.
C
C     FONCTIONS INTRINSEQUES:
C       NONE.
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       30/04/04 (OB): CREATION.
C----------------------------------------------------------------------
C RESPONSABLE BOITEAU O.BOITEAU
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      CHARACTER*8 SDFET1

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      
C DECLARATION VARIABLES LOCALES
      INTEGER MAXSD,MAXMA,MAXCH,MAXNO,MAXINT,MAXVOI
      PARAMETER(MAXSD=4,MAXMA=100,MAXCH=2,MAXNO=100,MAXINT=100,
     &          MAXVOI=10)
      INTEGER     NBMA(MAXSD),LISTMA(MAXMA,MAXSD),NBNO(MAXSD),
     &   LISTNO(2*MAXNO,MAXSD),NBVO(MAXSD),
     &   LISTCO(3*MAXVOI*MAXSD,2),NBFETE,LISTFE(MAXSD*MAXINT),
     &   LISTNI(4*MAXINT),LISTNJ(MAXINT*MAXVOI),NBPR(MAXSD),
     &   LISTPR(2*MAXINT,MAXSD),NBDDL(MAXSD)
      INTEGER      JADR,I,J,NBSD,INTBUF,NBNOIN,IFM,NIV,NBMAI,NBCHAR,
     &             NBNOI4,NBNOG,NBFETJ
      CHARACTER*8  K8BID,K8BUFF,NOMSD(MAXSD),NOMCHA(MAXCH),NOMO
      CHARACTER*19 SDFETI
      CHARACTER*24 NOMSDA,NOMSDB,NOMSDI,NOMSDG,NOMSDM,
     &  NOMSDH,NOMREF,NOMSDJ
      CHARACTER*32 JEXNOM
      
C CORPS DU PROGRAMME
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)

C NOM DU MODELE
      NOMO='MODM'
C NBRE DE CHARGE
      NBCHAR=1
C LISTE DES CHARGES
      NOMCHA(1)='CH1'
              
C NOMS DES SOUS-DOMAINES (8 CHARACTER ONLY/3 PREMIERS DEJA 
C DISCRIMINANTS POUR NUMERO.F LIGRSD)
      NOMSD(1)='SD1'
      NOMSD(2)='SD2'
      NOMSD(3)='SD3'
      NOMSD(4)='SD4'
      
C****************************************************************
C 4 SD 2D B (TPLLO1Z)
C****************************************************************
      CONTINUE
      NBSD=4
      NBNOG=19
      
C NBRE DE MAILLES PAR SOUS-DOMAINE (.FETA)     
      NBMA(1)=8
      NBMA(2)=8
      NBMA(3)=10
      NBMA(4)=10
                
C LISTE DES MAILLES PAR SOUS-DOMAINE (.FETA)      
      LISTMA(1,1)=1
      LISTMA(2,1)=2
      LISTMA(3,1)=3
      LISTMA(4,1)=4
      LISTMA(5,1)=5
      LISTMA(6,1)=6
C Pour maille-points
      LISTMA(7,1)=25
      LISTMA(8,1)=27
                        
      LISTMA(1,2)=19
      LISTMA(2,2)=20
      LISTMA(3,2)=21
      LISTMA(4,2)=22
      LISTMA(5,2)=23
      LISTMA(6,2)=24            
C Pour maille-points
      LISTMA(7,2)=29
      LISTMA(8,2)=31

      LISTMA(1,3)=13
      LISTMA(2,3)=14
      LISTMA(3,3)=15
      LISTMA(4,3)=16
      LISTMA(5,3)=17
      LISTMA(6,3)=18            
C Pour maille-points
      LISTMA(7,3)=30
      LISTMA(8,3)=32
C Mailles de peau
      LISTMA(9,3)=33
      LISTMA(10,3)=34

      LISTMA(1,4)=7
      LISTMA(2,4)=8
      LISTMA(3,4)=9
      LISTMA(4,4)=10
      LISTMA(5,4)=11
      LISTMA(6,4)=12      
C Pour maille-points
      LISTMA(7,4)=26
      LISTMA(8,4)=28
C Mailles de peau
      LISTMA(9,4)=35
      LISTMA(10,4)=36

C NBRE DE NOEUDS PAR SOUS-DOMAINE (.FETB)      
      NBNO(1)=7
      NBNO(2)=7
      NBNO(3)=7
      NBNO(4)=7
C LISTE DES NOEUDS ET DES DDLS CUMULES PAR SOUS-DOMAINE (.FETB)      
      LISTNO(1,1)=-1
      LISTNO(2,1)=2           
      LISTNO(3,1)=-3
      LISTNO(4,1)=4           
      LISTNO(5,1)=-17
      LISTNO(6,1)=6           
      LISTNO(7,1)=-4
      LISTNO(8,1)=8          
      LISTNO(9,1)=2
      LISTNO(10,1)=10
      LISTNO(11,1)=14
      LISTNO(12,1)=12
      LISTNO(13,1)=10
      LISTNO(14,1)=14
                  
      LISTNO(1,2)=-1
      LISTNO(2,2)=2           
      LISTNO(3,2)=15
      LISTNO(4,2)=4           
      LISTNO(5,2)=9
      LISTNO(6,2)=6           
      LISTNO(7,2)=-8
      LISTNO(8,2)=8
      LISTNO(9,2)=-16
      LISTNO(10,2)=10
      LISTNO(11,2)=-3
      LISTNO(12,2)=12
      LISTNO(13,2)=11
      LISTNO(14,2)=14
                       
      LISTNO(1,3)=-3
      LISTNO(2,3)=2           
      LISTNO(3,3)=-16
      LISTNO(4,3)=4
      LISTNO(5,3)=-8
      LISTNO(6,3)=6           
      LISTNO(7,3)=7
      LISTNO(8,3)=8      
      LISTNO(9,3)=18
      LISTNO(10,3)=10
      LISTNO(11,3)=-5
      LISTNO(12,3)=12      
      LISTNO(13,3)=12
      LISTNO(14,3)=14
                  
      LISTNO(1,4)=-3
      LISTNO(2,4)=2           
      LISTNO(3,4)=-5
      LISTNO(4,4)=4
      LISTNO(5,4)=19
      LISTNO(6,4)=6           
      LISTNO(7,4)=6
      LISTNO(8,4)=8
      LISTNO(9,4)=-4
      LISTNO(10,4)=10
      LISTNO(11,4)=-17
      LISTNO(12,4)=12
      LISTNO(13,4)=13
      LISTNO(14,4)=14            
C LISTE DES NOEUDS D'INTERFACE (.FETI)
      NBNOIN=10 
      LISTNI(1)=1
      LISTNI(2)=2
      LISTNI(3)=2
      LISTNI(4)=1
            
      LISTNI(5)=8
      LISTNI(6)=2
      LISTNI(7)=4
      LISTNI(8)=3
            
      LISTNI(9)=16
      LISTNI(10)=2
      LISTNI(11)=6
      LISTNI(12)=5

      LISTNI(13)=3
      LISTNI(14)=4
      LISTNI(15)=8
      LISTNI(16)=7

      LISTNI(17)=3
      LISTNI(18)=4
      LISTNI(19)=10
      LISTNI(20)=9
      
      LISTNI(21)=3
      LISTNI(22)=4
      LISTNI(23)=12
      LISTNI(24)=11

      LISTNI(25)=3
      LISTNI(26)=4
      LISTNI(27)=14
      LISTNI(28)=13
      
      LISTNI(29)=5
      LISTNI(30)=2
      LISTNI(31)=16
      LISTNI(32)=15
      
      LISTNI(33)=17
      LISTNI(34)=2
      LISTNI(35)=18
      LISTNI(36)=17

      LISTNI(37)=4
      LISTNI(38)=2
      LISTNI(39)=20
      LISTNI(40)=19                              
C LISTE DES SOUS-DOMAINES COMMUNS (.FETJ)
      NBFETJ=20
      LISTNJ(1)=1
      LISTNJ(2)=2
      LISTNJ(3)=2
      LISTNJ(4)=3
      LISTNJ(5)=2
      LISTNJ(6)=3      
      LISTNJ(7)=1
      LISTNJ(8)=2
      LISTNJ(9)=2
      LISTNJ(10)=3      
      LISTNJ(11)=3
      LISTNJ(12)=4
      LISTNJ(13)=1
      LISTNJ(14)=4      
      LISTNJ(15)=3
      LISTNJ(16)=4
      LISTNJ(17)=1      
      LISTNJ(18)=4
      LISTNJ(19)=1
      LISTNJ(20)=4                                          
C NBRE DE NOEUDS FRONTIERE POUR PRED/REST (.FETG)   
      NBPR(1)=5
      NBPR(2)=5
      NBPR(3)=5
      NBPR(4)=5
C LISTE DES ATTRIBUTS ASSOCIES (.FETG)     
      LISTPR(1,1)=-1
      LISTPR(2,1)=1     
      LISTPR(3,1)=-4
      LISTPR(4,1)=2      
      LISTPR(5,1)=-7
      LISTPR(6,1)=2     
      LISTPR(7,1)=-9
      LISTPR(8,1)=3
      LISTPR(9,1)=-10
      LISTPR(10,1)=4
                              
      LISTPR(1,2)=1
      LISTPR(2,2)=1      
      LISTPR(3,2)=4
      LISTPR(4,2)=6
      LISTPR(5,2)=-5
      LISTPR(6,2)=6            
      LISTPR(7,2)=-3
      LISTPR(8,2)=5
      LISTPR(9,2)=-2
      LISTPR(10,2)=4
      
      LISTPR(1,3)=-8
      LISTPR(2,3)=6      
      LISTPR(3,3)=5
      LISTPR(4,3)=1      
      LISTPR(5,3)=-6
      LISTPR(6,3)=1
      LISTPR(7,3)=3
      LISTPR(8,3)=2
      LISTPR(9,3)=2
      LISTPR(10,3)=3
            
      LISTPR(1,4)=8
      LISTPR(2,4)=2      
      LISTPR(3,4)=6
      LISTPR(4,4)=1      
      LISTPR(5,4)=7
      LISTPR(6,4)=1
      LISTPR(7,4)=9
      LISTPR(8,4)=6
      LISTPR(9,4)=10
      LISTPR(10,4)=5                  
C NOMBRE DE DDLS PAR SOUS-DOMAINES (.FETH)
      NBDDL(1)=14
      NBDDL(2)=14
      NBDDL(3)=14
      NBDDL(4)=14
                  
C****************************************************************
C ON STOCKE DANS SD_FETI
C****************************************************************      
C NUMERO MAXIMAL DE MAILLE      
      CONTINUE
      NBMAI=0
      DO 10 I=1,NBSD
        NBMAI=NBMAI+NBMA(I)
   10 CONTINUE
                                                       
C INITIALISATIONS
      SDFETI=SDFET1
      NOMREF=SDFETI//'.REFE'            
      NOMSDM=SDFETI//'.DIME'      
      NOMSDA=SDFETI//'.FETA'
      NOMSDB=SDFETI//'.FETB'
      NOMSDI=SDFETI//'.FETI'
      NOMSDJ=SDFETI//'.FETJ'      
      NOMSDG=SDFETI//'.FETG'
      NOMSDH=SDFETI//'.FETH'
      
C CREATION DES DIFFERENTS ATTRIBUTS DE LA S.D. SD_FETI
C .REFE
      INTBUF=NBCHAR+1
      CALL WKVECT(NOMREF,'G V K8',INTBUF,JADR)
      ZK8(JADR)=NOMO
      DO 50 I=1,NBCHAR
        ZK8(JADR+I)=NOMCHA(I)
   50 CONTINUE

      IF (NIV.GE.3) THEN 
        WRITE(IFM,*)
        WRITE(IFM,*)'DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD'      
        WRITE (IFM,*)'<FETI/OBTEMP> CREATION OBJET JEVEUX ',NOMREF 
      ENDIF 
      
C .DIME                 
      CALL WKVECT(NOMSDM,'G V I',5,JADR)
      ZI(JADR)=NBSD
      ZI(JADR+1)=NBNOIN
      ZI(JADR+2)=NBMAI
      ZI(JADR+3)=NBNOIN*2
      ZI(JADR+4)=NBNOG
            
      IF (NIV.GE.3) THEN 
        WRITE(IFM,*)
        WRITE(IFM,*)'DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD'      
        WRITE (IFM,*)'<FETI/OBTEMP> CREATION OBJET JEVEUX ',NOMSDM 
      ENDIF 
C .FETA
      CALL JECREC(NOMSDA,'G V I','NO','DISPERSE','VARIABLE',NBSD)
      DO 100 I=1,NBSD
        K8BUFF=NOMSD(I)
        CALL JECROC(JEXNOM(NOMSDA,K8BUFF))
        INTBUF=NBMA(I)
        CALL JEECRA(JEXNOM(NOMSDA,K8BUFF),'LONMAX',INTBUF,K8BID)
        CALL JEVEUO(JEXNOM(NOMSDA,K8BUFF),'E',JADR)
        DO 90 J=1,INTBUF
          ZI(JADR+J-1)=LISTMA(J,I)
   90   CONTINUE                        
  100 CONTINUE
      IF (NIV.GE.3) 
     &  WRITE (IFM,*)'<FETI/OBTEMP> CREATION OBJET JEVEUX ',NOMSDA   
  
C .FETB
      CALL JECREC(NOMSDB,'G V I','NO','DISPERSE','VARIABLE',NBSD)
      DO 200 I=1,NBSD
        K8BUFF=NOMSD(I)      
        CALL JECROC(JEXNOM(NOMSDB,K8BUFF))
        INTBUF=2*NBNO(I)
        CALL JEECRA(JEXNOM(NOMSDB,K8BUFF),'LONMAX',INTBUF,K8BID)
        CALL JEVEUO(JEXNOM(NOMSDB,K8BUFF),'E',JADR)
        DO 210 J=1,INTBUF
          ZI(JADR+J-1)=LISTNO(J,I)
  210   CONTINUE                        
  200 CONTINUE
      IF (NIV.GE.3) 
     &  WRITE (IFM,*)'<FETI/OBTEMP> CREATION OBJET JEVEUX ',NOMSDB   
       
     
C .FETI
      NBNOI4=4*NBNOIN                  
      CALL WKVECT(NOMSDI,'G V I',NBNOI4,JADR)
      DO 500 I=1,NBNOI4
        ZI(JADR+I-1)=LISTNI(I)
  500 CONTINUE
      IF (NIV.GE.3) 
     &  WRITE (IFM,*)'<FETI/OBTEMP> CREATION OBJET JEVEUX ',NOMSDI   

C .FETJ
      CALL WKVECT(NOMSDJ,'G V I',NBFETJ,JADR)
      DO 550 I=1,NBFETJ
        ZI(JADR+I-1)=LISTNJ(I)
  550 CONTINUE
      IF (NIV.GE.3) 
     &  WRITE (IFM,*)'CREATION OBJET JEVEUX ',NOMSDJ
                    
C .FETG
      CALL JECREC(NOMSDG,'G V I','NO','DISPERSE','VARIABLE',NBSD)
      DO 600 I=1,NBSD
        K8BUFF=NOMSD(I)      
        CALL JECROC(JEXNOM(NOMSDG,K8BUFF))
        INTBUF=2*NBPR(I)
        CALL JEECRA(JEXNOM(NOMSDG,K8BUFF),'LONMAX',INTBUF,K8BID)
        CALL JEVEUO(JEXNOM(NOMSDG,K8BUFF),'E',JADR)
        DO 610 J=1,INTBUF
          ZI(JADR+J-1)=LISTPR(J,I)
  610   CONTINUE                        
  600 CONTINUE
      IF (NIV.GE.3) 
     &  WRITE (IFM,*)'<FETI/OBTEMP> CREATION OBJET JEVEUX ',NOMSDG
        
C.FETH
      CALL WKVECT(NOMSDH,'G V I',NBSD,JADR)
      DO 700 I=1,NBSD
        ZI(JADR+I-1)=NBDDL(I)
  700 CONTINUE
      IF (NIV.GE.3) 
     &  WRITE (IFM,*)'<FETI/OBTEMP> CREATION OBJET JEVEUX ',NOMSDH
        
      IF (NIV.GE.4)
     &  CALL UTIMSD(IFM,2,.FALSE.,.TRUE.,SDFETI(1:19),1,' ')
      IF (NIV.GE.3) THEN        
        WRITE(IFM,*)'DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD'
        WRITE(IFM,*)
      ENDIF
                
      CALL JEDEMA()
      END
