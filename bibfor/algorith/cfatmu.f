      SUBROUTINE CFATMU (NEQ,NESMAX,NDIM,NBLIAC,
     &                   FROT,LLF,LLF1,LLF2,RESOCO)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/12/2009   AUTEUR ABBAS M.ABBAS 
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
      IMPLICIT     NONE
      INTEGER      NEQ
      INTEGER      NESMAX
      INTEGER      NDIM
      INTEGER      NBLIAC
      INTEGER      LLF
      INTEGER      LLF1
      INTEGER      LLF2
      INTEGER      FROT
      CHARACTER*24 RESOCO
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE DISCRETE - ALGORITHME)
C
C CALCUL DE ATMU - VECTEUR DES FORCES DE CONTACT
C
C
C ----------------------------------------------------------------------
C  
C
C IN  NEQ    : NOMBRE D'EQUATIONS
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NESMAX : NOMBRE MAX DE NOEUDS ESCLAVES
C              (SERT A DECALER LES POINTEURS POUR LE FROTTEMENT 3D)
C IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
C IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
C                'E': RESOCO(1:14)//'.ATMU'
C IN  LLF    : NOMBRE DE LIAISONS DE FROTTEMENT (EN 2D)
C              NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LES DEUX
C               DIRECTIONS SIMULTANEES (EN 3D)
C IN  LLF1   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
C               PREMIERE DIRECTION (EN 3D)
C IN  LLF2   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
C               SECONDE DIRECTION (EN 3D)
C IN  FROT   : VAUT 1 LORSQU'IL Y A DU FROTTEMENT, 0 SINON
C
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
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
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER       BTOTAL,ILIAC,LLIAC,JDECAL
      INTEGER       POSIT,NBDDL
      INTEGER       DEKLAG,COMPTS,COMPT0,COMPT1,COMPT2,KK
      CHARACTER*19  LIAC,MU,ATMU
      INTEGER       JLIAC,JMU,JATMU
      CHARACTER*24  APPOIN,APDDL,APCOEF,APCOFR
      INTEGER       JAPPTR,JAPDDL,JAPCOE,JAPCOF
C ======================================================================
      CALL JEMARQ ()
C ======================================================================
C --- APPEL JEVEUX POUR LA MISE A JOUR DES VECTEURS DE LIAISONS --------
C ======================================================================
      LIAC   = RESOCO(1:14)//'.LIAC'
      MU     = RESOCO(1:14)//'.MU'
      ATMU   = RESOCO(1:14)//'.ATMU'
      APPOIN = RESOCO(1:14)//'.APPOIN'
      APDDL  = RESOCO(1:14)//'.APDDL'
      APCOEF = RESOCO(1:14)//'.APCOEF'
      APCOFR = RESOCO(1:14)//'.APCOFR'
      CALL JEVEUO (LIAC,  'L',JLIAC )
      CALL JEVEUO (MU,    'L',JMU   )
      CALL JEVEUO (APPOIN,'L',JAPPTR)
      CALL JEVEUO (APDDL, 'L',JAPDDL)
      CALL JEVEUO (APCOEF,'L',JAPCOE)
      CALL JEVEUO (ATMU,  'E',JATMU )


      IF (FROT.EQ.1) THEN
        CALL JEVEUO (APCOFR,'L',JAPCOF)
      ENDIF
C
C --- INITIALISATION
C
      DO 170 KK = 1,NEQ
        ZR(JATMU+KK-1) = 0.0D0
  170 CONTINUE

C ======================================================================
C --- CALCUL DE AT.MU --------------------------------------------------
C ======================================================================
      BTOTAL = NBLIAC + LLF + LLF1 + LLF2
      DEKLAG = 0
      COMPTS = 0
      COMPT0 = NBLIAC
      COMPT1 = NBLIAC + (NDIM-1)*LLF
      COMPT2 = NBLIAC + (NDIM-1)*LLF + LLF1
      DO 10 ILIAC = 1, BTOTAL
         LLIAC  = ZI(JLIAC +ILIAC-1)
         JDECAL = ZI(JAPPTR+LLIAC-1)
         NBDDL  = ZI(JAPPTR+LLIAC  ) - ZI(JAPPTR+LLIAC-1)
         CALL CFTYLI(RESOCO, ILIAC, POSIT)
         GOTO (1000, 2000, 3000, 4000) POSIT
 1000    CONTINUE
C ======================================================================
C --- CAS D'UNE LIAISON DE CONTACT -------------------------------------
C ======================================================================
         COMPTS = COMPTS + 1
         CALL CALATM (NEQ,NBDDL,ZR(JMU-1+COMPTS),
     &                   ZR(JAPCOE+JDECAL),ZI(JAPDDL+JDECAL),ZR(JATMU))
         GOTO 10
 2000    CONTINUE
C ======================================================================
C --- CAS D'UNE LIAISON DE FROTTEMENT ADHERENT SUIVANT LES DEUX --------
C --- DIRECTION OU DANS LE CAS GENERAL EN 2D ---------------------------
C ======================================================================
         COMPT0 = COMPT0 + 1
         CALL CALATM(NEQ,NBDDL,ZR(JMU-1+COMPT0),
     &                   ZR(JAPCOF+JDECAL),ZI(JAPDDL+JDECAL),ZR(JATMU))
         IF (NDIM.EQ.3) THEN
            DEKLAG = DEKLAG + 1
            CALL CALATM(NEQ,NBDDL,ZR(JMU-1+COMPT0+LLF),
     &         ZR(JAPCOF+JDECAL+30*NESMAX),ZI(JAPDDL+JDECAL),ZR(JATMU))
         ENDIF
         IF (FROT.EQ.0) THEN
           CALL ASSERT(.FALSE.)
         ENDIF
         GOTO 10
 3000    CONTINUE
C ======================================================================
C --- CAS D'UNE LIAISON DE FROTTEMENT ADHERENT SUIVANT LA PREMIERE -----
C --- DIRECTION --------------------------------------------------------
C ======================================================================
         COMPT1 = COMPT1 + 1
         CALL CALATM(NEQ,NBDDL,ZR(JMU-1+COMPT1),
     &                   ZR(JAPCOF+JDECAL),ZI(JAPDDL+JDECAL),ZR(JATMU))
         IF (FROT.EQ.0) THEN
           CALL ASSERT(.FALSE.)
         ENDIF
         GOTO 10
 4000    CONTINUE
C ======================================================================
C --- CAS D'UNE LIAISON DE FROTTEMENT ADHERENT SUIVANT LA SECONDE ------
C --- DIRECTION --------------------------------------------------------
C ======================================================================
         COMPT2 = COMPT2 + 1
         CALL CALATM(NEQ,NBDDL,ZR(JMU-1+COMPT2),
     &          ZR(JAPCOF+JDECAL+30*NESMAX),ZI(JAPDDL+JDECAL),ZR(JATMU))
         IF (FROT.EQ.0) THEN
           CALL ASSERT(.FALSE.)
         ENDIF
 10   CONTINUE
C ======================================================================
      CALL JEDEMA ()
C ======================================================================
      END
