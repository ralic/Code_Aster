      SUBROUTINE GFFORC ( CHGRFL, Z, DZ, D2Z, DT, FPMEC, FFMEC,
     +                    FFPLAQ, FPTG1, FFTG1, FPTG2, FFTG2, FRTG2
     &                    , PRESCR, P0)
      IMPLICIT NONE
      CHARACTER*24        CHGRFL
      REAL*8              Z, DZ, D2Z, DT, FPMEC, FFMEC, 
     +                    FFPLAQ, FPTG1, FFTG1, FPTG2, FFTG2, FRTG2
      REAL*8              PRESCR
C ----------------------------------------------------------------------
C TOLE CRP_20
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/11/2007   AUTEUR BOYERE E.BOYERE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C BUT : CALCUL DES FORCES FLUIDES NON LINEAIRES S'EXERCANT SUR LES
C       GRAPPES LORS DE LA CHUTE DE CELLES-CI 
C
C   ARGUMENT        E/S  TYPE         ROLE
C    NOMCHA         IN    K8        CHARGE CONTENANT LES DONNEES 
C                                   POUR LE CALCUL DES FORCES FLUIDES
C                                   S'EXERCANT SUR LA GRAPPE
C    Z              IN    R         POSITION DU BAS DU CRAYON
C    DZ             IN    R         VITESSE  DU BAS DU CRAYON
C    D2Z            IN    R         ACCELERATIOM DU BAS DU CRAYON
C    DT             IN    R         PAS DE TEMPS
C    IT             IN    I         NUMERO D'ITERATION
C    FPMEC          OUT   R         PRESSION EN HAUT DE LA TIGE
C                                   DE COMMANDE
C    FFMEC          OUT   R         FORCE REPARTIE DANS LE MECANISME
C                                   DE COMMANDE
C    FFPLAQ         OUT   R         FORCE REPARTIE DANS LE GUIDAGE
C                                   DE COMMANDE
C    FPTG1          OUT   R         PRESSION A LA BASE DU CRAYON
C    FFTG1          OUT   R         FORCE REPARTIE DANS LE TUBE
C                                   GUIDE AVANT DASHPOT
C    FPTG2          OUT   R         PRESSION A LA BASE DU CRAYON
C    FFTG2          OUT   R         FORCE REPARTIE DANS LE TUBE
C                                   GUIDE APRES DASHPOT
C    FRTG2          OUT   R         FORCE REPARTIE DANS LE TUBE
C                                   GUIDE APRES DASHPOT
C
C.========================= DEBUT DES DECLARATIONS ====================
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
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
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER      I1, I2, I3, I5, I6, I7, I8, I9, I10, I11, I12,
     +             I13, I14, I15, I16, I19
      INTEGER      II, JFFL, JIFL, IT, I
      REAL*8       CGG, ROP, Q, Z0, L1, L2, L3, ROC, ROD, AC, 
     +             LTIGE, VARAI, LCRAY, DTM, PIS4, R8PI
      LOGICAL      DEBUG
      REAL*8       X(5), Y(6)
      REAL*8       P0
C
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEMARQ()
      PIS4 = 0.250D0 * R8PI()
      DEBUG  = .FALSE.
CCC      DEBUG  = .TRUE.
C
      CALL JEVEUO ( '&&GFLECT.INDICE', 'E', JIFL ) 
      CALL JEVEUO ( CHGRFL, 'E', JFFL ) 
C
      IT = ZI(JIFL-1+3)
      ZI(JIFL-1+3) = IT + 1
C
      IF ( DEBUG ) THEN
         WRITE(6,*)'*******  IT  = ', IT
         WRITE(6,*)'*******  DT  = ', DT
         WRITE(6,*)'*******  Z   = ', Z
         WRITE(6,*)'*******  DZ  = ', DZ
         WRITE(6,*)'*******  D2Z = ', D2Z
      ENDIF
C
      FPMEC  = 0.D0
      FFMEC  = 0.D0
      FFPLAQ = 0.D0
      FPTG1  = 0.D0
      FFTG1  = 0.D0
      FPTG2  = 0.D0
      FFTG2  = 0.D0
      FRTG2  = 0.D0
C
      II = 5 + ZI(JIFL-1+5)
      I1 = ZI(JIFL-1+II+1)
      I2 = ZI(JIFL-1+II+2)
      I3 = ZI(JIFL-1+II+3)
      I5 = ZI(JIFL-1+II+5)
      I6 = ZI(JIFL-1+II+6)
      I7 = ZI(JIFL-1+II+7)
      I8 = ZI(JIFL-1+II+8)
      I9 = ZI(JIFL-1+II+9)
      I10 = ZI(JIFL-1+II+10)
      I11 = ZI(JIFL-1+II+11)
      I12 = ZI(JIFL-1+II+12)
      I13 = ZI(JIFL-1+II+13)
      I14 = ZI(JIFL-1+II+14)
      I15 = ZI(JIFL-1+II+15)
      I16 = ZI(JIFL-1+II+16)
      I19 = ZI(JIFL-1+II+19)
C
      Z0 = ZR(JFFL-1+I11+4)
      L2 = ZR(JFFL-1+I7+15)
      L3 = ZR(JFFL-1+I7+16)
C
      ZR(JFFL-1+I14+1) = Z
      ZR(JFFL-1+I14+2) = DZ
      ZR(JFFL-1+I14+4) = ZR(JFFL-1+I14+3)
      ZR(JFFL-1+I14+3) = D2Z
      ZR(JFFL-1+I14+5) = DT
C
C --- FORCE DE PLAQUAGE DANS LE GUIDAGE CONTINU :
C     -----------------------------------------
      CGG = ZR(JFFL-1+I5+1)
      ROP = ZR(JFFL-1+I3+3)
      Q   = ZR(JFFL-1+I3+8)
      FFPLAQ = CGG*ROP*Q**2

      IF ( DEBUG ) THEN
         WRITE(6,*)' '
         WRITE(6,*)'******* FORCE DE PLAQUAGE '
         WRITE(6,*)'          FORCE PRESSION = ', FFPLAQ
      ENDIF

C
C --- FORCE REPARTIE DANS LE MECANISME DE COMMANDE :
C     --------------------------------------------
      CALL GFCOMM ( IT, Z, DZ, D2Z, DT, FPMEC, FFMEC, ZR(JFFL-1+1),
     +            ZR(JFFL-1+I2+1),  ZR(JFFL-1+I3+1),  ZR(JFFL-1+I6+1),
     +            ZR(JFFL-1+I9+1),  ZR(JFFL-1+I10+1), ZR(JFFL-1+I15+1),
     +            ZR(JFFL-1+I12+1), ZR(JFFL-1+I13+1), ZR(JFFL-1+I8+1) )

      IF ( DEBUG ) THEN
         WRITE(6,*)' '
         WRITE(6,*)'******* FORCE MECANISME DE COMMANDE '
         WRITE(6,*)'                    FORCE PRESSION = ', FPMEC
         WRITE(6,*)'                   FORCE VISQUEUSE = ', FFMEC
      ENDIF


      L1 = ZR(JFFL-1+I7+14)
      Z0 = ZR(JFFL-1+I11+4)

      IF ( (Z+Z0) .LT. L1 ) THEN
C
C ------ FORCE REPARTIE DANS LE TUBE GUIDE AVANT DASHPOT :
C        -----------------------------------------------
         CALL GFGUID ( IT, Z, DZ, FPTG1, FFTG1, ZR(JFFL-1+I3+1),
     +                ZR(JFFL-1+I7+1), ZR(JFFL-1+I1+1),
     +                ZI(JIFL-1+1), ZR(JFFL-1+I12+1), ZR(JFFL-1+I16+1),
     +                Z0 ) 

         DO 111 I=1,5
            X(I) = ZR(JFFL-1+I16+1+I)
 111     CONTINUE

         IF ( DEBUG ) THEN
            WRITE(6,*)' '
            WRITE(6,*)'******* FORCE TUBE GUIDE AVANT DASHPOT '
            WRITE(6,*)'                    FORCE PRESSION = ', FPTG1
            WRITE(6,*)'                   FORCE VISQUEUSE = ', FFTG1
         ENDIF

      ELSE 
C
C ------ FORCE REPARTIE DANS LE TUBE GUIDE APRES DASHPOT :
C        -----------------------------------------------
         CALL GFDASH ( IT, Z, DZ, D2Z, DT, FPTG2, FFTG2, FRTG2, 
     +              ZR(JFFL-1+I3+1), ZR(JFFL-1+I7+1), ZR(JFFL-1+I1+1),
     +              ZR(JFFL-1+I12+1), ZR(JFFL-1+I16+1),
     +              ZR(JFFL-1+I19+1), ZI(JIFL-1+4), Z0, L2, L3 ,Y)

         IF ( DEBUG ) THEN
            WRITE(6,*)' '
            WRITE(6,*)'******* FORCE TUBE GUIDE APRES DASHPOT '
            WRITE(6,*)'                    FORCE PRESSION = ', FPTG2
            WRITE(6,*)'                   FORCE VISQUEUSE = ', FFTG2
            WRITE(6,*)'                  DANS LE RETRIENT = ', FRTG2
         ENDIF

      ENDIF 


      IF ( (Z+Z0) .LT. L1 ) THEN
       PRESCR = X(4)
      ELSE
       PRESCR = Y(5)
      ENDIF
      
      P0 = ZR(JFFL-1+I3+5)

C --- CALCUL DE LA MASSE APPARENTE :
C     ----------------------------
      ROC    = ZR(JFFL-1+I3+1)
      ROD    = ZR(JFFL-1+I3+2)
      AC     = ZR(JFFL-1+I7+8)
      LTIGE  = ZR(JFFL-1+I9+3)
      VARAI  = ZR(JFFL-1+I9+4)
      LCRAY  = ZR(JFFL-1+I9+7)
      DTM    = ZR(JFFL-1+I9+8)

C --- ON CALCULE ARCHIMEDE :
C     ----------------------
      ZR(JFFL-1+I8+2) =   PIS4*DTM**2*(ROD*(LTIGE-Z) + ROP*Z)
     +                  + 24*AC*(ROC*Z + ROP*(LCRAY-Z)) + VARAI*ROP
C
      CALL JEDEMA()
C.============================ FIN DE LA ROUTINE ======================
      END
