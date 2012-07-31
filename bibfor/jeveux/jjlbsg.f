      SUBROUTINE JJLBSG (IC, ID, IOC, IBACOL, IADMI, IADYN, LTOT) 
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 30/07/2012   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C ----------------------------------------------------------------------
C 
C LIBERE LE SEGMENT DE VALEURS ASSOCIES A UN OBJET SIMPLE OU UN OBJET
C DE COLLECTION DISPERSEE
C
C IN     IC   : CLASSE ASSOCIEE A L'OBJET
C IN     ID   : IDENTIFICATEUR D'OBJET SIMPLE OU DE COLLECTIOB
C IN    IOC   : IDENTIFICATUER DE COLLECTION
C IN IBACOL   ; ADDRESSE DU DESCRIPTEUR DE COLLECTION
C IN  IADMI   : ADDRESSE DE L'OBJET A LIBERER
C IN  IADYN   : ADDRESSE DYNAMIQUE DE L'OBJET A LIBERER
C OUT   LTOT  : LONGUEUR EN ENTIERS LIBEREE
C
C ----------------------------------------------------------------------
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
C TOLE CRP_18 CRS_505 CRS_508 CRS_512
      IMPLICIT NONE
      INTEGER            IC, ID, IOC, IBACOL, IADMI, IADYN, LTOT  
C     ------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C-----------------------------------------------------------------------
      INTEGER IACCE ,IBIADD ,IBIADM ,IBID ,IBLONO ,IXDESO ,IXIADD 
      INTEGER IXIADM ,IXLONO ,JCARA ,JDATE ,JHCOD ,JIACCE ,JIADD 
      INTEGER JIADM ,JLONG ,JLONO ,JLTYP ,JLUTI ,JMARQ ,LONOI 
      INTEGER LSV ,N ,NBACCE 
C-----------------------------------------------------------------------
      PARAMETER  ( N = 5 )
      INTEGER          LTYP    , LONG    , DATE    , IADD    , IADM    ,
     +                 LONO    , HCOD    , CARA    , LUTI    , IMARQ
      COMMON /IATRJE/  LTYP(1) , LONG(1) , DATE(1) , IADD(1) , IADM(1) ,
     +                 LONO(1) , HCOD(1) , CARA(1) , LUTI(1) , IMARQ(1)
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     +                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
C
      INTEGER          NBLMAX    , NBLUTI    , LONGBL    ,
     &                 KITLEC    , KITECR    ,             KIADM    ,
     &                 IITLEC    , IITECR    , NITECR    , KMARQ
      COMMON /IFICJE/  NBLMAX(N) , NBLUTI(N) , LONGBL(N) ,
     &                 KITLEC(N) , KITECR(N) ,             KIADM(N) ,
     &                 IITLEC(N) , IITECR(N) , NITECR(N) , KMARQ(N)
      COMMON /IACCED/  IACCE(1)
      COMMON /JIACCE/  JIACCE(N),NBACCE(2*N)
      INTEGER          LDYN , LGDYN , NBDYN , NBFREE
      COMMON /IDYNJE/  LDYN , LGDYN , NBDYN , NBFREE
      INTEGER          ICDYN , MXLTOT
      COMMON /XDYNJE/  ICDYN , MXLTOT
      REAL *8         MXDYN, MCDYN, MLDYN, VMXDYN, VMET, LGIO 
      COMMON /R8DYJE/ MXDYN, MCDYN, MLDYN, VMXDYN, VMET, LGIO(2)
      INTEGER          ISSTAT
      COMMON /ICONJE/  ISSTAT
      INTEGER          LBIS , LOIS , LOLS , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOR8 , LOC8
C ----------------------------------------------------------------------
      INTEGER        IVNMAX     , IDDESO     , IDIADD    , IDIADM     ,
     +               IDMARQ     , IDNOM      ,             IDLONG     ,
     +               IDLONO     , IDLUTI     , IDNUM
      PARAMETER    ( IVNMAX = 0 , IDDESO = 1 , IDIADD = 2 , IDIADM = 3 ,
     +               IDMARQ = 4 , IDNOM  = 5 ,              IDLONG = 7 ,
     +               IDLONO = 8 , IDLUTI = 9  ,IDNUM  = 10 )
C ----------------------------------------------------------------------
      INTEGER        IDM, ISD, ISF, IL, LTYPI, LGS, IADDI(2),NBIOAV(2)
C      
      NBIOAV(1) = NBACCE(2*IC-1)
      NBIOAV(2) = NBACCE(2*IC  )
C           
      IF (IOC .EQ. 0) THEN
C
C     ON TRAITE UN OBJET SIMPLE OU UN OBJET $$DESO
C
        IF (IADYN .NE. 0) THEN
          IDM  = IADMI - 4
          ISD  = ISZON(JISZON + IDM + 3) / ISSTAT
          ISF  = ISZON(JISZON + ISZON(JISZON+IDM) - 4) / ISSTAT
          IL = ISZON(JISZON+IDM) - 8 - IDM
          IF ( ISD .EQ. 1 ) THEN
C
C     LE SEGMENT DE VALEURS EST MARQUE X A OU X D, ON PEUT LE LIBERER
C
            LTYPI = LTYP( JLTYP(IC)+ID )
            LSV   = LONO( JLONO(IC)+ID ) * LTYPI
            IF ( ISF .EQ. 4 ) THEN
C
C     LE SEGMENT DE VALEURS EST MARQUE X D, IL FAUT D'ABORD L'ECRIRE
C
              IADDI(1) = IADD ( JIADD(IC)+2*ID-1 )
              IADDI(2) = IADD ( JIADD(IC)+2*ID   )
              CALL JXECRO ( IC, IADMI, IADDI, LSV, 0, ID)
              IADD( JIADD(IC)+2*ID-1 ) = IADDI(1)
              IADD( JIADD(IC)+2*ID   ) = IADDI(2)
            ENDIF
            LGS = ISZON(JISZON+IADMI-4) - IADMI + 4
            MCDYN = MCDYN - LGS*LOIS
            MLDYN = MLDYN + LGS*LOIS
            CALL HPDEALLC ( IADYN , NBFREE , IBID )
            LTOT = LTOT + IL
            IADM(JIADM(IC)+2*ID-1) = 0
            IADM(JIADM(IC)+2*ID  ) = 0
          ENDIF
        ENDIF
      ELSE
C
C     ON TRAITE UN OBJET DE COLLECTION DISPERSEE
C
        IXIADM = ISZON ( JISZON + IBACOL + IDIADM )
        IXIADD = ISZON ( JISZON + IBACOL + IDIADD )
        IXDESO = ISZON ( JISZON + IBACOL + IDDESO )
        IXLONO = ISZON ( JISZON + IBACOL + IDLONO )
        IF (IXIADM .GT. 0) THEN
          IBIADM = IADM ( JIADM(IC) + 2*IXIADM-1 )
          IBIADD = IADM ( JIADM(IC) + 2*IXIADD-1 )
          IF (IADYN .NE. 0) THEN
            IDM  = IADMI - 4
            ISD  = ISZON(JISZON + IDM + 3) / ISSTAT
            ISF  = ISZON(JISZON + ISZON(JISZON+IDM) - 4) / ISSTAT
            IL = ISZON(JISZON+IDM) - 8 - IDM
            IF ( ISD .EQ. 1 ) THEN
C
C     LE SEGMENT DE VALEURS EST MARQUE X A OU X D, ON PEUT LE LIBERER
C
              IF ( IXLONO .NE. 0 ) THEN
                IBLONO = IADM ( JIADM(IC) + 2*IXLONO-1 )
                LONOI = ISZON(JISZON + IBLONO - 1 + IOC)
              ELSE
                LONOI = LONO(JLONO(IC)+ IXDESO)
              ENDIF
              LTYPI = LTYP( JLTYP(IC)+IXDESO )
              LSV   = LONOI * LTYPI
              IF ( ISF .EQ. 4 ) THEN
C
C     LE SEGMENT DE VALEURS EST MARQUE X D, IL FAUT D'ABORD L'ECRIRE
C
                IADDI(1) = ISZON(JISZON + IBIADD -1 + 2*IOC-1)
                IADDI(2) = ISZON(JISZON + IBIADD -1 + 2*IOC  )
                CALL JXECRO ( IC, IADMI, IADDI, LSV, ID, IOC)
                ISZON(JISZON + IBIADD -1 + 2*IOC-1) = IADDI(1)
                ISZON(JISZON + IBIADD -1 + 2*IOC  ) = IADDI(2)
              ENDIF
              LGS = ISZON(JISZON+IADMI-4) - IADMI + 4
              MCDYN = MCDYN - LGS*LOIS
              MLDYN = MLDYN + LGS*LOIS
              CALL HPDEALLC ( IADYN , NBFREE , IBID )
              LTOT = LTOT + IL
              ISZON(JISZON + IBIADM - 1 +2*IOC-1) = 0
              ISZON(JISZON + IBIADM - 1 +2*IOC  ) = 0
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C      
      LGIO(1)=LGIO(1)+1024*LONGBL(IC)*LOIS*(NBACCE(2*IC-1)-NBIOAV(1)) 
      LGIO(2)=LGIO(2)+1024*LONGBL(IC)*LOIS*(NBACCE(2*IC  )-NBIOAV(2)) 
C
      MXLTOT=MXLTOT+(LTOT*LOIS)/(1024*1024)
C
      END
