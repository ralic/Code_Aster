      SUBROUTINE OP5902(NBOCCP,IFM,NIV,COMPOR)
      IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C RESPONSABLE JMBHH01 J.M.PROIX

C     COMMANDE:  DEFI_COMPOR MOT-CLE POLYCRISTAL

      INCLUDE 'jeveux.h'
      CHARACTER*8 COMPOR, MONO
      CHARACTER*16 KBID, LOCA
      REAL*8 FVOL,ORIE(3),DL,DA,EULER(3),FVOLT
      INTEGER IOCC,NLOC,NBOCCP,NDL,NDA,ITBINT
      INTEGER I,NMONO,IMK,IMI,IPK,IPI,IPR,IORIE
      INTEGER NCPRI,NCPRK,NCPRR,JCPRK,JCPRR,JCPRI,NVIT,LMK,IFVOL,IPL
      INTEGER IMONO,NBMONO,INDIK8,NVLOC
      INTEGER NBTBSY,NBFSYS,DECAL,IMR,IFA,NBSYST
      INTEGER IARG,IFM,NIV    



      CALL JEMARQ()

      CALL GETVTX(' ','LOCALISATION',0,IARG,1,LOCA,NLOC)
      DL=0.D0
      DA=0.D0
      NVLOC=0
      IF (LOCA.EQ.'BETA') THEN
        CALL GETVR8(' ','DL',0,IARG,1,DL,NDL)
        CALL GETVR8(' ','DA',0,IARG,1,DA,NDA)
        NVLOC=2
      ENDIF
      
      
C     organisation de CPRI :
C     1 : TYPE =2 pour POLYCRISTAL
C     2 : NBPHAS pour POLYCRISTAL
C     3 : NVITOT pour POLYCRISTAL
C     4 : NOMBRE DE MONOCRISTAUX différents
C     5 : NBFAMILLES DE SYS GLIS pour Phase 1
C     6 : Numero du MONO 1
C     7 : NVI du Mono 1
C     8 : NBFAMILLES DE SYS GLIS pour Phase 2
C     9 : Numero du MONO 2
C     10 : NVI du Mono 2
C      etc...
C     avant dernier : dimension de CPRK
C     nombre de paramètres de localisation

      NCPRI=4+3*NBOCCP+1+1+1
      CALL WKVECT(COMPOR//'.CPRI', 'G V I',NCPRI,IPI)
      ZI(IPI)=2
      ZI(IPI+1)=NBOCCP
      CALL WKVECT('&&OP0059.LISTEMONO','V V K8',NBOCCP,IPL)
      
      NBMONO=0
      NCPRK=0
      
      DO 13 IOCC=1,NBOCCP
         CALL GETVID('POLYCRISTAL','MONOCRISTAL',IOCC,IARG,1,MONO,
     &                NMONO)
C        On ne stocke pas les doublons
         IMONO=INDIK8(ZK8(IPL),MONO,1,NBMONO)
         IF (IMONO.EQ.0) THEN
            NBMONO=NBMONO+1
            ZK8(IPL-1+NBMONO)=MONO
            ZI(IPI-1+4+3*(IOCC-1)+2)=NBMONO
            CALL JELIRA(MONO//'.CPRK','LONMAX',LMK,KBID)
            NCPRK=NCPRK+LMK+2
         ELSE
            ZI(IPI-1+4+3*(IOCC-1)+2)=IMONO
         ENDIF
  13  CONTINUE
      NCPRK=NCPRK+1
      IF (NBMONO.GT.5) THEN
         CALL U2MESG('F','COMPOR2_16',0,' ',1,ITBINT,0,0.D0)
      ELSE
         ZI(IPI-1+4)=NBMONO
      ENDIF
      
C     organisation de CPRK :
C     On ne stocke que les monocristaux DIFFERENTS
C     1   : Nom méthode localisation
C     2   : Nom Monocristal 1 + NBFAM + CPRK du monocristal 1
C     n+2 : Nom Monocristal 2 + NBFAM + CPRK du monocristal 2
C     ..: etc...
      CALL WKVECT(COMPOR//'.CPRK', 'G V K16',NCPRK,IPK)
      JCPRK=1
      ITBINT=0
      DO 15 IMONO=1,NBMONO
         MONO=ZK8(IPL-1+IMONO)
         CALL JELIRA(MONO//'.CPRK','LONMAX',LMK,KBID)
         CALL JEVEUO(MONO//'.CPRK','L',IMK)
         CALL JEVEUO(MONO//'.CPRI','L',IMI)
C        RECOPIE DU VECTEUR K16 DU MONOCRISTAL DANS CELUI DU POLY
         ZK16(IPK-1+JCPRK+1)=MONO
         WRITE(ZK16(IPK-1+JCPRK+2),'(I16)') ZI(IMI-1+5)
         DO 14 I=1,LMK
            ZK16(IPK-1+JCPRK+2+I)=ZK16(IMK-1+I)
 14      CONTINUE
         JCPRK=JCPRK+LMK+2
         
 15   CONTINUE
 
      NCPRR=4*NBOCCP+2

      CALL WKVECT(COMPOR//'.CPRR', 'G V R',NCPRR,IPR)
      JCPRR=0
      JCPRI=4
      NVIT=0
      FVOLT=0.D0
      DO 16 IOCC=1,NBOCCP
         IMONO=ZI(IPI-1+4+3*(IOCC-1)+2)
         MONO=ZK8(IPL-1+IMONO)
         CALL JEVEUO(MONO//'.CPRI','L',IMI)
         ZI(IPI-1+JCPRI+1)=ZI(IMI-1+5)
         ZI(IPI-1+JCPRI+3)=ZI(IMI-1+7)
         
C        NOMBRE DE VAR INT MONO + 6 (TENSEUR BETA OU EPSG)
C        On enlève 3 v.i. de chaque monocristal
C        nombre de variables internes par phase
C        6+3*Ns+6 = (Evp + Ns(alphas, gammas, ps) +  Sig)

         NVIT=NVIT-3+ZI(IMI-1+7)+6
         JCPRI=JCPRI+3
         CALL GETVR8('POLYCRISTAL','FRAC_VOL',IOCC,IARG,1,FVOL,
     &                IFVOL)
         CALL GETVR8('POLYCRISTAL','ANGL_REP',IOCC,IARG,3,ORIE,
     &                IORIE)
         IF (IORIE.EQ.0) THEN
             CALL GETVR8('POLYCRISTAL','ANGL_EULER',IOCC,IARG,3,
     &                   EULER,
     &                IORIE)
             CALL EULNAU(EULER,ORIE)
         ENDIF
         FVOLT=FVOLT+FVOL
         ZR(IPR-1+JCPRR+1)=FVOL
         ZR(IPR-1+JCPRR+2)=ORIE(1)
         ZR(IPR-1+JCPRR+3)=ORIE(2)
         ZR(IPR-1+JCPRR+4)=ORIE(3)
         JCPRR=JCPRR+4
 16   CONTINUE
 
      IF (ABS(FVOLT-1.D0).GT.1.D-3) THEN
         CALL U2MESR ('F', 'COMPOR2_8', 1, FVOLT)
      ENDIF
      
      ZR(IPR-1+JCPRR+1)=DL
      ZR(IPR-1+JCPRR+2)=DA
     
C      NOMBRE DE VAR INT TOTAL + 8 (TENSEUR B OU EVP + NORME+INDIC)
      ZI(IPI-1+3)=NVIT+8
      ZI(IPI-1+NCPRI-2)=JCPRK
      ZI(IPI-1+NCPRI-1)=NVLOC
      
      ZK16(IPK)=LOCA
      IF (NIV.EQ.2) THEN
         WRITE(IFM,*) ' NOMBRE DE PHASES ',NBOCCP
         WRITE(IFM,*) ' NOMBRE DE MONOCRISTAUX DIFFERENTS ',NBMONO
      ENDIF
C FIN ------------------------------------------------------------------
      CALL JEDEMA()
      END
