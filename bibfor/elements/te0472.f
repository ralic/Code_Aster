      SUBROUTINE TE0472 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 02/09/2002   AUTEUR UFBHHLL C.CHAVANT 
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
C TOLE CRP_20
C
C     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
C          CORRESPONDANT A UN FLUX THM (THH, THHM, THH, HHM,HM)
C          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 2D
C
C          OPTION : 'CHAR_MECA_FLUX_R'
C          OPTION : 'CHAR_MECA_FLUX_F'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
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
C
      CHARACTER*8        NOMPAR(3),ELREFE,KBID
      CHARACTER*24       CARAC,FF
      REAL*8             POIDS,R,Z,TX,TY,NX,NY,VALPAR(3),DELTAT,TPLUS
      REAL*8             PRES,PRESF
C
      REAL*8             FLU1,FLU2,FLUTH
      INTEGER            NAPRE1,NAPRE2,NATEMP
C
      INTEGER            NNO,NNOS,KP,NPG,ICARAC
      INTEGER            IFF,IPOIDS,IVF,IDFDE,IGEOM
      INTEGER            IPRES,K,I,L,IRES,IFLUX,ITEMPS,IOPT,IPRESF
      INTEGER            IS,IM,LS,LM
      INTEGER            IDLTHM,NDLTHM,DEBTHM
      INTEGER            IFLUXF,IRET,NDLNO
      LOGICAL            AXI,DPLAN,TRAITE
C
      LOGICAL VOL2,BORD2,VOL3,BORD3
      LOGICAL ISTHT3
      LOGICAL ISTHQ4
      LOGICAL ISTHT6
      LOGICAL ISTHQ8
      LOGICAL ISTHS2
      LOGICAL ISTHS3
      LOGICAL ISTHF8,ISTHF6
C
      LOGICAL ISTH10,ISTH13,ISTH15,ISTH20
C
      INTEGER NNOMAX,NVOMAX,NSOMAX
      PARAMETER(NNOMAX=20,NVOMAX=4,NSOMAX=8)
      INTEGER VOISIN(NVOMAX,NNOMAX)
      INTEGER NBVOS(NSOMAX)
C
      LOGICAL P2P1,LUMPED
C     ------------------------------------------------------------------
C
      CALL ELREF1(ELREFE)
      AXI = .FALSE.
      TRAITE = .FALSE.
      DPLAN = .FALSE.
      BORD2 = .TRUE.
      VOL2  = .FALSE.
      BORD3 = .FALSE.
      VOL3  = .FALSE.
C
       CALL CAETHM(NOMTE,VOL2,BORD2,VOL3,BORD3,
     > KBID,AXI,DPLAN,TRAITE,
     > ISTHS2,ISTHS3,ISTHF8,ISTHF6,ISTH10,ISTH13,ISTH15,ISTH20,
     > ISTHT3,ISTHQ4,ISTHT6,ISTHQ8,
     > NNOMAX,NVOMAX,NSOMAX,
     > NNOS,VOISIN,NBVOS,P2P1,LUMPED)




C     ------------------------------------------------------------------
C  CETTE ROUTINE FAIT UN CALCUL EN THHM , HM , HHM , THH THM
C     ------------------------------------------------------------------
C        SI MODELISATION = THHM
         IF (NOMTE(1:4).EQ.'THHM') THEN
            NDLNO=5
C
C        SI MODELISATION = HM
         ELSEIF (NOMTE(1:2).EQ.'HM') THEN
            NDLNO=3
C
C SI MODELISATION = HHM
         ELSEIF (NOMTE(1:3).EQ.'HHM') THEN
            NDLNO=4
C
C SI MODELISATION = THH
         ELSEIF (NOMTE(1:4).EQ.'THH_') THEN
            NDLNO=3
C
C SI MODELISATION = THV
         ELSEIF (NOMTE(1:4).EQ.'THV_') THEN
            NDLNO=2
C
C SI MODELISATION = THM
         ELSEIF (NOMTE(1:4).EQ.'THM_') THEN
            NDLNO=4
C
         ELSE
            CALL UTMESS('F','TE0472','ELEMENT '
     +      // 'NON TRAITE')
         ENDIF
C
C
      CARAC = '&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE ( CARAC, 'L', ICARAC )
      NNO = ZI(ICARAC)
      NPG = ZI(ICARAC+2)
C
      FF = '&INEL.'//ELREFE//'.FF'
      CALL JEVETE ( FF, 'L', IFF )
      IPOIDS = IFF
      IVF    = IPOIDS+NPG
      IDFDE  = IVF   +NPG*NNO

      CALL JEVECH ( 'PGEOMER', 'L', IGEOM )
      CALL JEVECH ( 'PVECTUR', 'E', IRES  )
C
C
      IF (OPTION.EQ.'CHAR_MECA_FLUX_R') THEN
         IOPT = 1
         CALL JEVECH ( 'PFLUXR' , 'L', IFLUX  )
         CALL JEVECH ( 'PTEMPSR', 'L', ITEMPS )
         DELTAT = ZR(ITEMPS+1)
C
      ELSE IF (OPTION.EQ.'CHAR_MECA_FLUX_F') THEN
         IOPT = 2
         CALL JEVECH ( 'PFLUXF' , 'L', IFLUXF )
         CALL JEVECH ( 'PTEMPSR', 'L', ITEMPS )
         TPLUS  = ZR(ITEMPS)
         DELTAT = ZR(ITEMPS+1)
         NOMPAR(1) = 'X'
         NOMPAR(2) = 'Y'
         NOMPAR(3) = 'INST'
         VALPAR(3) = TPLUS
C
      ELSE IF (OPTION.EQ.'CHAR_MECA_PRES_R') THEN
         IOPT = 3
         CALL JEVECH ( 'PPRESSR', 'L', IPRES )
C
      ELSE IF (OPTION.EQ.'CHAR_MECA_PRES_F') THEN
         IOPT = 4
         CALL JEVECH ( 'PPRESSF', 'L', IPRESF )
         CALL JEVECH ( 'PTEMPSR' ,'L', ITEMPS )
         NOMPAR(1) = 'X'
         NOMPAR(2) = 'Y'
         NOMPAR(3) = 'INST'
         VALPAR(3) = ZR(ITEMPS)
C
      END IF
C
C    BOUCLE SUR LES POINTS DE GAUSS
C
      DO 100 KP = 1 , NPG
         K = (KP-1)*NNO
C
         CALL VFF2DN ( NNO, ZR(IPOIDS+KP-1), ZR(IDFDE+K), ZR(IGEOM),
     &                 NX, NY, POIDS )
C
C        --- OPTION CHAR_MECA_FLUX_F ---
C
         IF ( AXI ) THEN
            R = 0.D0
            Z = 0.D0
            DO 104 I = 1 , NNO
               L = (KP-1)*NNO+I
               R = R + ZR(IGEOM+2*I-2) * ZR(IVF+L-1)
 104        CONTINUE
            POIDS = POIDS*R
         ENDIF
C
C
C        --- OPTION CHAR_MECA_FLUX_R OU CHAR_MECA_FLUX_F ---
C
      IF ( IOPT.EQ.1.OR.IOPT.EQ.2) THEN
C
C SI MODELISATION = THHM OU THH
C
        IF (NOMTE(1:4).EQ.'THHM'.OR.NOMTE(1:4).EQ.'THH_') THEN
C
C --- NAPRE1,NAPRE2,NATEMP SONT MIS EN PLACE
C --- POUR UNE EVENTUELLE MODIFICATION DE L'ORDRE DES DDL :
C     PRE1, PRE2, TEMP DANS LES CATALOGUES D'ELEMENTS
C
                 NAPRE1=0
                 NAPRE2=1
                 NATEMP=2
C
            IF (IOPT.EQ.1) THEN
C
C ---   FLUTH REPRESENTE LE FLUX THERMIQUE
C ---   FLU1 REPRESENTE LE FLUX ASSOCIE A PRE1
C ---   FLU2 REPRESENTE LE FLUX ASSOCIE A PRE2
C
               FLU1  = ZR((IFLUX)+(KP-1)*3+NAPRE1  )
               FLU2  = ZR((IFLUX)+(KP-1)*3+NAPRE2  )
               FLUTH = ZR((IFLUX)+(KP-1)*3+NATEMP  )
C
            ELSE IF (IOPT.EQ.2) THEN
               R = 0.D0
               Z = 0.D0
               DO 203 I = 1 , NNO
                  L = (KP-1)*NNO+I
                  R = R + ZR(IGEOM+2*I-2) * ZR(IVF+L-1)
                  Z = Z + ZR(IGEOM+2*I-1) * ZR(IVF+L-1)
 203           CONTINUE
               VALPAR(1) = R
               VALPAR(2) = Z
              CALL FOINTE('FM',ZK8(IFLUXF+NAPRE1),3,
     &                     NOMPAR,VALPAR,FLU1,IRET)
              CALL FOINTE('FM',ZK8(IFLUXF+NAPRE2),3,
     &                    NOMPAR,VALPAR,FLU2,IRET)
              CALL FOINTE('FM',ZK8(IFLUXF+NATEMP),3,
     &                    NOMPAR,VALPAR,FLUTH,IRET)
            ENDIF
            IF (NOMTE(1:4).EQ.'THHM')THEN
              DO 305 I = 1 , NNO
                L = 5 * (I-1) -1
                ZR(IRES+L+3) = ZR(IRES+L+3) - POIDS *
     &                                  DELTAT * FLU1 * ZR(IVF+K+I-1)
                ZR(IRES+L+4) = ZR(IRES+L+4) - POIDS *
     &                                  DELTAT * FLU2 * ZR(IVF+K+I-1)
                ZR(IRES+L+5) = ZR(IRES+L+5) - POIDS *
     &                                  DELTAT * FLUTH * ZR(IVF+K+I-1)
 305          CONTINUE
                NDLTHM = 3
                DEBTHM = 3
            ELSE
              DO 307 I = 1 , NNO
                L = 3 * (I-1) -1
                ZR(IRES+L+1) = ZR(IRES+L+1) - POIDS *
     &                                  DELTAT * FLU1 * ZR(IVF+K+I-1)
                ZR(IRES+L+2) = ZR(IRES+L+2) - POIDS *
     &                                  DELTAT * FLU2 * ZR(IVF+K+I-1)
                ZR(IRES+L+3) = ZR(IRES+L+3) - POIDS *
     &                                  DELTAT * FLUTH * ZR(IVF+K+I-1)
 307          CONTINUE
                NDLTHM = 3
                DEBTHM = 1
            ENDIF
C
         ENDIF
C
C SI MODELISATION = THV
C
        IF (NOMTE(1:4).EQ.'THV_') THEN
C
C --- NAPRE1,NAPRE2,NATEMP SONT MIS EN PLACE
C --- POUR UNE EVENTUELLE MODIFICATION DE L'ORDRE DES DDL :
C     PRE1, PRE2, TEMP DANS LES CATALOGUES D'ELEMENTS
C
                 NAPRE1=0
                 NATEMP=1
C
            IF (IOPT.EQ.1) THEN
C
C ---   FLUTH REPRESENTE LE FLUX THERMIQUE
C ---   FLU1 REPRESENTE LE FLUX ASSOCIE A PRE1
C
               FLU1  = ZR((IFLUX)+(KP-1)*2+NAPRE1  )
               FLUTH = ZR((IFLUX)+(KP-1)*2+NATEMP  )
C
            ELSE IF (IOPT.EQ.2) THEN
               R = 0.D0
               Z = 0.D0
               DO 204 I = 1 , NNO
                  L = (KP-1)*NNO+I
                  R = R + ZR(IGEOM+2*I-2) * ZR(IVF+L-1)
                  Z = Z + ZR(IGEOM+2*I-1) * ZR(IVF+L-1)
 204           CONTINUE
               VALPAR(1) = R
               VALPAR(2) = Z
              CALL FOINTE('FM',ZK8(IFLUXF+NAPRE1),3,
     &                     NOMPAR,VALPAR,FLU1,IRET)
              CALL FOINTE('FM',ZK8(IFLUXF+NAPRE2),3,
     &                    NOMPAR,VALPAR,FLU2,IRET)
              CALL FOINTE('FM',ZK8(IFLUXF+NATEMP),3,
     &                    NOMPAR,VALPAR,FLUTH,IRET)
            ENDIF
              DO 205 I = 1 , NNO
                L = 2 * (I-1) -1
                ZR(IRES+L+1) = ZR(IRES+L+1) - POIDS *
     &                                  DELTAT * FLU1 * ZR(IVF+K+I-1)
                ZR(IRES+L+2) = ZR(IRES+L+2) - POIDS *
     &                                  DELTAT * FLUTH * ZR(IVF+K+I-1)
 205          CONTINUE
                NDLTHM = 2
                DEBTHM = 1
C
         ENDIF
C
C SI MODELISATION = HM
         IF (NOMTE(1:2).EQ.'HM') THEN
C
                 NAPRE1=0
C
            IF (IOPT.EQ.1) THEN
C
C ---   FLU1 REPRESENTE LE FLUX ASSOCIE A PRE1
C
               FLU1 = ZR((IFLUX)+(KP-1)+NAPRE1 )
C
            ELSE IF (IOPT.EQ.2) THEN
               R = 0.D0
               Z = 0.D0
               DO 102 I = 1 , NNO
                  L = (KP-1)*NNO+I
                  R = R + ZR(IGEOM+2*I-2) * ZR(IVF+L-1)
                  Z = Z + ZR(IGEOM+2*I-1) * ZR(IVF+L-1)
 102           CONTINUE
               VALPAR(1) = R
               VALPAR(2) = Z
              CALL FOINTE('FM',ZK8(IFLUXF+NAPRE1),3,
     &                    NOMPAR,VALPAR,FLU1,IRET)
            ENDIF
            DO 308 I = 1 , NNO
              L = 3 * (I-1) -1
              ZR(IRES+L+3) = ZR(IRES+L+3) - POIDS *
     &                       DELTAT * FLU1 * ZR(IVF+K+I-1)
 308        CONTINUE
                NDLTHM = 1
                DEBTHM = 3
C
         ENDIF
C
C SI MODELISATION = HHM
C
         IF (NOMTE(1:3).EQ.'HHM') THEN
C
             NAPRE1=0
             NAPRE2=1
C
             IF (IOPT.EQ.1) THEN
C
C ---   FLU1 REPRESENTE LE FLUX ASSOCIE A PRE1
C ---   FLU2 REPRESENTE LE FLUX ASSOCIE A PRE2
C
                FLU1 = ZR((IFLUX)+(KP-1)*2+NAPRE1 )
                FLU2 = ZR((IFLUX)+(KP-1)*2+NAPRE2 )
C
             ELSE IF (IOPT.EQ.2) THEN
                R = 0.D0
                Z = 0.D0
                DO 202 I = 1 , NNO
                   L = (KP-1)*NNO+I
                   R = R + ZR(IGEOM+2*I-2) * ZR(IVF+L-1)
                   Z = Z + ZR(IGEOM+2*I-1) * ZR(IVF+L-1)
 202            CONTINUE
                VALPAR(1) = R
                VALPAR(2) = Z
                CALL FOINTE('FM',ZK8(IFLUXF+NAPRE1),3,
     &                      NOMPAR,VALPAR,FLU1,IRET)
                CALL FOINTE('FM',ZK8(IFLUXF+NAPRE2),3,
     &                      NOMPAR,VALPAR,FLU2,IRET)
              ENDIF
              DO 304 I = 1 , NNO
                L = 4 * (I-1) -1
                ZR(IRES+L+3) = ZR(IRES+L+3) - POIDS *
     &                               DELTAT * FLU1 * ZR(IVF+K+I-1)
                ZR(IRES+L+4) = ZR(IRES+L+4) - POIDS *
     &                                DELTAT * FLU2 * ZR(IVF+K+I-1)
 304           CONTINUE
                NDLTHM = 2
                DEBTHM = 3
C
         ENDIF
C
C
C SI MODELISATION = THM
C
         IF (NOMTE(1:3).EQ.'THM') THEN
C
C --- NAPRE1,NAPRE2,NATEMP SONT MIS EN PLACE
C --- POUR UNE EVENTUELLE MODIFICATION DE L'ORDRE DES DDL :
C     PRE1, PRE2, TEMP DANS LES CATALOGUES D'ELEMENTS
C
                 NAPRE1=0
                 NATEMP=1
C
            IF (IOPT.EQ.1) THEN
C
C ---   FLUTH REPRESENTE LE FLUX THERMIQUE
C ---   FLU1 REPRESENTE LE FLUX ASSOCIE A PRE1
C ---   FLU2 REPRESENTE LE FLUX ASSOCIE A PRE2
C
               FLU1  = ZR((IFLUX)+(KP-1)*2+NAPRE1  )
               FLUTH = ZR((IFLUX)+(KP-1)*2+NATEMP  )
C
            ELSE IF (IOPT.EQ.2) THEN
               R = 0.D0
               Z = 0.D0
               DO 103 I = 1 , NNO
                  L = (KP-1)*NNO+I
                  R = R + ZR(IGEOM+2*I-2) * ZR(IVF+L-1)
                  Z = Z + ZR(IGEOM+2*I-1) * ZR(IVF+L-1)
 103           CONTINUE
               VALPAR(1) = R
               VALPAR(2) = Z
               CALL FOINTE('FM',ZK8(IFLUXF+NAPRE1),3,
     &                     NOMPAR,VALPAR,FLU1,IRET)
               CALL FOINTE('FM',ZK8(IFLUXF+NATEMP),3,
     &                    NOMPAR,VALPAR,FLUTH,IRET)
            ENDIF
            DO 309 I = 1 , NNO
                L = 4 * (I-1) -1
                ZR(IRES+L+3) = ZR(IRES+L+3) - POIDS *
     &                                  DELTAT * FLU1 * ZR(IVF+K+I-1)
                ZR(IRES+L+4) = ZR(IRES+L+4) - POIDS *
     &                                  DELTAT * FLUTH * ZR(IVF+K+I-1)
 309        CONTINUE

                NDLTHM = 2
                DEBTHM = 3
C
         ENDIF
C
C TRAITEMENT P2P1
C
                IF ( P2P1) THEN
                 DO 220 IM = NNOS+1,NNO
                  IS = VOISIN(1,IM)
                  LS = NDLNO * (IS-1) -1
                  LM = NDLNO * (IM-1) -1
                  DO 221 IDLTHM = 1, NDLTHM
                   ZR(IRES+LS+DEBTHM+IDLTHM-1) =
     >             ZR(IRES+LS+DEBTHM+IDLTHM-1) +
     >             ZR(IRES+LM+DEBTHM+IDLTHM-1)/2.D0
  221             CONTINUE
                  IS = VOISIN(2,IM)
                  LS = NDLNO * (IS-1) -1
                  DO 222 IDLTHM = 1, NDLTHM
                   ZR(IRES+LS+DEBTHM+IDLTHM-1) =
     >             ZR(IRES+LS+DEBTHM+IDLTHM-1) +
     >             ZR(IRES+LM+DEBTHM+IDLTHM-1)/2.D0
                   ZR(IRES+LM+DEBTHM+IDLTHM-1) = 0.D0
  222             CONTINUE
  220           CONTINUE
               ENDIF
C
C        --- OPTION CHAR_MECA_PRES_R OU CHAR_MECA_PRES_F ---
C
         ELSE IF ( (IOPT.EQ.3) .OR. (IOPT.EQ.4) ) THEN
            IF (IOPT.EQ.3) THEN
               PRES = 0.D0
               DO 106 I = 1 , NNO
                  L = (KP-1)*NNO+I
                  PRES = PRES + ZR(IPRES+I-1)*ZR(IVF+L-1)

 106           CONTINUE
            ELSE IF (IOPT.EQ.4) THEN
               PRES = 0.D0
               DO 108 I = 1 , NNO
                  VALPAR(1) = ZR(IGEOM+2*I-2)
                  VALPAR(2) = ZR(IGEOM+2*I-1)
                CALL FOINTE('FM',ZK8(IPRESF),3,NOMPAR,VALPAR,PRESF,IRET)
                  L = (KP-1)*NNO+I
                  PRES = PRES + PRESF*ZR(IVF+L-1)
 108           CONTINUE
            ENDIF
C
            TX = -NX* PRES
            TY = -NY* PRES

            DO 303 I = 1 , NNO

               L = NDLNO * (I-1) -1
               ZR(IRES+L+1) = ZR(IRES+L+1) + TX*ZR(IVF+K+I-1)*POIDS
               ZR(IRES+L+2) = ZR(IRES+L+2) + TY*ZR(IVF+K+I-1)*POIDS

 303        CONTINUE
        ENDIF
C
 100   CONTINUE
C
      END
