subroutine i3idks(epsi, k, desc, desctm, sgt,&
                  conexk, coordo, nbpt, lstpt)
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/i3ctpv.h"
#include "asterfort/i3ifqs.h"
#include "asterfort/i3ifts.h"
#include "asterfort/utmess.h"
    integer :: k, desc(*), desctm(*), conexk(*), nbpt, lstpt(*)
    real(kind=8) :: epsi, sgt(*), coordo(*)
!
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     INTERSECTION FRONTIERE DE K SGT (AB)
!     ------------------------------------------------------------------
! IN  EPSI   : R : PRECISION
! IN  K      : I : -
! IN  DESC   : I :  !--> OBJ MAILLE POINTEE (ET CE QU' ELLE POINTE)
! IN  DESCTM : I : -
! IN  CONEXK : I : CONNECTIVITE DE LA MAILLE POINTEE
! IN  COORDO : R : TABLE GLOBALE DES COORDONEES
! IN  SGT    : R : COORDONNEES DES POINTS A ET B -
! OUT NBPT   : I : NOMBRE DE POINT TROUVE
!            :   : CONVENTION NBPT = -2 <=> CARD(INTER) = INFINI
!            :   : DANS CE CAS OUT = EXTREMITES
! OUT LSTPT  : I : OBJ LISTE_POINT
!     ------------------------------------------------------------------
!     STRUCT LISTE_POINT
!             ( REEL          ABSC      (NMAXPT)
!               ENTIER        FACE      (NMAXPT)
!               ENTIER        ARETE     (NMAXPT)
!              (PLANE,GAUCHE) TYPE_FACE (NMAXPT)
!               REEL          COORDO_REF(NMAXPT)
!               ENTIER        ORDRE     (NMAXPT)
!             );
!     STRUCT LISTE_POINT LSTPT;
!     ------------------------------------------------------------------
!
!
!
    integer :: ndglof(4), nbndf, nbf, i, decf, ndloc, adescm, face
    integer :: vali(2)
    aster_logical :: fink, nonvid
!
!======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    nbpt = 0
    face = 0
    fink = .false.
    adescm = desctm(desc(k))
    nbf = zi(adescm)
100 continue
    if (.not. fink) then
        face = face + 1
        nbndf = zi(adescm-1 + 2 + face)
        decf = 8 + face
        do 10 i = 1, nbndf, 1
            ndloc = zi(adescm -1 + decf + (i-1)*6)
            ndglof(i) = conexk(ndloc)
 10     continue
        call i3ctpv(epsi, ndglof, nbndf, coordo, sgt,&
                    nonvid)
        if (nonvid) then
            if (nbndf .eq. 3) then
                call i3ifts(epsi, k, face, desc, desctm,&
                            conexk, coordo, sgt, nbpt, lstpt,&
                            fink)
            else if (nbndf .eq. 4) then
                call i3ifqs(epsi, k, face, desc, desctm,&
                            conexk, coordo, sgt, nbpt, lstpt,&
                            fink)
            else
                vali (1) = face
                vali (2) = k
                call utmess('F', 'INTEMAIL_23', ni=2, vali=vali)
            endif
        endif
        fink = ( fink .or. (face .eq. nbf) )
        goto 100
    endif
end subroutine
