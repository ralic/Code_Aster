subroutine dyexre(numddl, freq, nbexre, exreco, exresu,&
                  j2nd)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit      none
#include "jeveux.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/vtcopy.h"
#include "asterfort/vtcreb.h"
    character(len=24) :: exreco, exresu
    integer :: nbexre
    character(len=14) :: numddl
    real(kind=8) :: freq
    integer :: j2nd
!
! ----------------------------------------------------------------------
!
! DYNA_LINE_HARM
!
! APPLICATION EXCIT_RESU
!
! ----------------------------------------------------------------------
!
!
! IN  NBEXRE : NOMBRE DE EXCIT_RESU
! IN  NUMDDL : NOM DU NUME_DDL
! IN  EXRECO : LISTE DES COEFFICIENTS DANS EXCIT_RESU
! IN  EXRESU : LISTE DES RESULTATS DANS EXCIT_RESU
! IN  NUMDDL : NOM DU NUME_DDL
! IN  FREQ   : VALEUR DE LA FREQUENCE
! IN  J2ND   : ADRESSE DU VECTEUR ASSEMBLE SECOND MEMBRE
!
!
!
!
    character(len=19) :: chamno, chamn2
    real(kind=8) :: prec, eps0
    integer :: ieq, neq, iresu, ibid, ifreq(1), iret
    character(len=8) :: k8bid
    integer :: jlccre, jlresu
    complex(kind=8) :: c16bid
    complex(kind=8), pointer :: vale(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    chamn2 = '&&DYEXRE.CHAMN2'
    call detrsd('CHAM_NO', chamn2)
    call vtcreb(chamn2, 'V', 'C',&
                nume_ddlz = numddl,&
                nb_equa_outz = neq)
    prec = 1.d-6
    eps0 = 1.d-12
    call jeveuo(exreco, 'L', jlccre)
    call jeveuo(exresu, 'L', jlresu)
    do iresu = 1, nbexre
        if (abs(freq) .gt. eps0) then
            call rsorac(zk8(jlresu+iresu-1), 'FREQ', 0, freq, k8bid,&
                        c16bid, prec, 'RELATIF', ifreq, 1,&
                        ibid)
        else
            call rsorac(zk8(jlresu+iresu-1), 'FREQ', 0, freq, k8bid,&
                        c16bid, eps0, 'ABSOLU', ifreq, 1,&
                        ibid)
        endif
        call rsexch('F', zk8(jlresu+iresu-1), 'DEPL', ifreq(1), chamno,&
                    iret)
        call vtcopy(chamno, chamn2, 'F', ibid)
        call jeveuo(chamn2//'.VALE', 'L', vc=vale)
        do ieq = 1, neq
            zc(j2nd-1+ieq) = zc(j2nd-1+ieq) + vale(ieq)*zc( jlccre-1+iresu)
        end do
    end do
!
    call jedema()
end subroutine
