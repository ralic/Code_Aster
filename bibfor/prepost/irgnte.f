      SUBROUTINE IRGNTE ( IFI, NBORDR, COORD, CONNEX, POINT, 
     +                    NJVMAI, NBMAI, CNSV, PARTIE, JTYPE, CNSD )
      IMPLICIT NONE
C
      INTEGER        NBHEX, IFI, NBORDR, CONNEX(*), POINT(*),
     +               CNSV(*), CNSD(*), JTYPE
      REAL*8         COORD(*)
      CHARACTER*(*)  NJVMAI,PARTIE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 14/06/2005   AUTEUR CIBHHPD L.SALMONA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     IMPRESSION D'UN CHAM_NO AU FORMAT GMSH :
C
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                     ZK24
      CHARACTER*32                              ZK32
      CHARACTER*80                                       ZK80
      COMMON /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER      IMAI, IMA, IPOIN, LISTNO(8), J, JCNSV,
     +             JCNSD, NCMP, K, JMAI, IOR, INOE, NBNO, NBMAI
      REAL*8       ZERO
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      ZERO = 0.0D0
C
      CALL JEVEUO ( NJVMAI, 'L', JMAI )
      IF (NJVMAI(10:12).EQ.'POI') THEN
        NBNO = 1
      ELSEIF (NJVMAI(10:12).EQ.'SEG') THEN
        NBNO = 2
      ELSEIF (NJVMAI(10:12).EQ.'TRI') THEN
        NBNO = 3
      ELSEIF (NJVMAI(10:12).EQ.'QUA') THEN
        NBNO = 4
      ELSEIF (NJVMAI(10:12).EQ.'TET') THEN
        NBNO = 4
      ELSEIF (NJVMAI(10:12).EQ.'PYR') THEN
        NBNO = 5
      ELSEIF (NJVMAI(10:12).EQ.'PRI') THEN
        NBNO = 6
      ELSEIF (NJVMAI(10:12).EQ.'HEX') THEN
        NBNO = 8
      ENDIF
C
      DO 10 IMAI = 1 , NBMAI

         IMA = ZI(JMAI-1+IMAI)

         IPOIN = POINT(IMA)

         DO 20 INOE = 1, NBNO

           LISTNO(INOE) = CONNEX(IPOIN + INOE - 1 )

 20      CONTINUE

         DO 12 J = 1 , 3          
            WRITE(IFI,1000) (COORD(3*(LISTNO(INOE)-1)+J),INOE=1,NBNO)
 12      CONTINUE
C
         DO 14 IOR = 1 , NBORDR

           JCNSV = CNSV(IOR)
           JCNSD = CNSD(IOR)
           NCMP  = ZI(JCNSD-1+2)
           IF (ZK8(JTYPE-1+IOR).EQ.'R') THEN        

             DO 16 INOE = 1 , NBNO

               IF (NJVMAI(10:12).EQ.'SEG'.OR.NJVMAI(10:12).EQ.'TRI'
     +         .OR.NJVMAI(10:12).EQ.'QUA') THEN

                 WRITE(IFI,1000) ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+1),
     +           ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+4), ZERO,  
     +           ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+4),
     +           ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+2), ZERO,
     +           ZERO ,ZERO ,ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+3)   

               ELSEIF (NJVMAI(10:12).EQ.'PYR'.OR.
     +         NJVMAI(10:12).EQ.'PRI'.OR.NJVMAI(10:12).EQ.'HEX') THEN

                 WRITE(IFI,1000) ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+1),
     +           ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+4),   
     +           ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+5),
     +           ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+4),
     +           ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+2),
     +           ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+6),
     +           ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+5),
     +           ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+6),
     +           ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+3)

               ENDIF

 16          CONTINUE

           ELSEIF (ZK8(JTYPE-1+IOR).EQ.'C') THEN        

             IF (PARTIE.EQ.'REEL') THEN

               DO 26 INOE = 1 , NBNO

                 IF (NJVMAI(10:12).EQ.'SEG'.OR.NJVMAI(10:12).EQ.'TRI'
     +           .OR.NJVMAI(10:12).EQ.'QUA') THEN

                   WRITE(IFI,1000) ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+1),
     +             ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+4), ZERO,  
     +             ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+4),
     +             ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+2), ZERO,
     +             ZERO ,ZERO ,ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+3)   

                 ELSEIF (NJVMAI(10:12).EQ.'PYR'.OR.
     +           NJVMAI(10:12).EQ.'PRI'.OR.NJVMAI(10:12).EQ.'HEX') THEN

                   WRITE(IFI,1000) ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+1),
     +             ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+4),   
     +             ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+5),
     +             ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+4),
     +             ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+2),
     +             ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+6),
     +             ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+5),
     +             ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+6),
     +             ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+3)

                 ENDIF

 26            CONTINUE
             ELSEIF (PARTIE.EQ.'IMAG') THEN
               DO 36 INOE = 1 , NBNO

                 IF (NJVMAI(10:12).EQ.'SEG'.OR.NJVMAI(10:12).EQ.'TRI'
     +           .OR.NJVMAI(10:12).EQ.'QUA') THEN

                   WRITE(IFI,1000) ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+1),
     +             ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+4), ZERO,  
     +             ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+4),
     +             ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+2), ZERO,
     +             ZERO ,ZERO ,ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+3)   

                 ELSEIF (NJVMAI(10:12).EQ.'PYR'.OR.
     +           NJVMAI(10:12).EQ.'PRI'.OR.NJVMAI(10:12).EQ.'HEX') THEN

                   WRITE(IFI,1000) ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+1),
     +             ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+4),   
     +             ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+5),
     +             ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+4),
     +             ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+2),
     +             ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+6),
     +             ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+5),
     +             ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+6),
     +             ZR(JCNSV-1+(LISTNO(INOE)-1)*NCMP+3)
                 ENDIF
 36            CONTINUE
             ENDIF
           ENDIF
 14      CONTINUE
 10   CONTINUE
C
      CALL JELIBE ( NJVMAI )
C
      CALL JEDEMA()
C
 1000 FORMAT(1P,9(E15.7E3,1X))
C
      END
