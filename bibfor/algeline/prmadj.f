        SUBROUTINE PRMADJ
     &             (NBND,NEQ,N2,ADJNCY,XADJ,XADJD,LISTE,P,Q,NOEUD)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 30/05/2002   AUTEUR JFBHHUC C.ROSE 
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
C RESPONSABLE JFBHHUC C.ROSE
        IMPLICIT NONE
        INTEGER NBND,NEQ,N2
        INTEGER ADJNCY(*),XADJ(NEQ),XADJD(*),LISTE(NEQ),P(NEQ),Q(N2)
        INTEGER NOEUD(*),NBNOEU,NDI,NDJ,DEB,FIN,DEBLIS,I,J,NDSUIV
        LOGICAL VIDER
        VIDER = .FALSE.
        NBNOEU = 0
        DEBLIS=0
        DO 50 I=1,NBND
        XADJD(I)=1
 50     CONTINUE
        DO 100 I=1,N2
           NDI = NOEUD(Q(I))
           DEB = XADJ(I)
           FIN = XADJ(I+1)-1
           DO 120 J = DEB , FIN
              NDJ =  NOEUD(Q(ADJNCY(J)))
              IF(NDI.NE.NDJ) THEN
C     ON MET  NDJ DANS  LA LISTE
                 CALL PRMADL(NDJ,DEBLIS,LISTE)
              ENDIF
 120       CONTINUE
C     ON ECRIT LA LISTE DANS ADJNCY,XADJD ET ON LA REMET A ZERO
           IF(I.EQ.N2) THEN
             VIDER = .TRUE.
           ELSE 
             NDSUIV = NOEUD(Q(I+1))
             IF(NDSUIV.NE.NDI) THEN
             NBNOEU = NBNOEU + 1
             VIDER = .TRUE.
             ENDIF
           ENDIF
           IF(VIDER) THEN
              CALL PRMADE(DEBLIS,LISTE,ADJNCY,XADJD,NDI)
              VIDER = .FALSE.
           ENDIF
 100    CONTINUE
        END
