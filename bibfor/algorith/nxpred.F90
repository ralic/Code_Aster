subroutine nxpred(modele, mate, carele, charge, infoch,&
                  numedd, solveu, lostat, time, lonch,&
                  matass, maprec, vtemp, vtempm, vtempp,&
                  vhydr, vhydrp, tmpchi, tmpchf, compor,&
                  cndirp, cnchci, vec2nd, vec2ni)
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
! person_in_charge: jessica.haelewyn at edf.fr
!
! aslint: disable=W1504
    implicit none
#include "jeveux.h"
#include "asterfort/asasve.h"
#include "asterfort/ascova.h"
#include "asterfort/copisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/resoud.h"
#include "asterfort/verstp.h"
#include "asterfort/vethbt.h"
#include "asterfort/vethbu.h"
    integer :: lonch
    character(len=19) :: solveu, maprec
    character(len=24) :: modele, mate, carele, charge, infoch, numedd, time
    character(len=24) :: matass, cndirp, cnchci, cnresi
    character(len=24) :: vtempm, vtempp, vtemp, vec2nd, vec2ni
    character(len=24) :: vhydr, vhydrp, compor, tmpchi, tmpchf
    logical :: lostat
!
! ----------------------------------------------------------------------
!
! COMMANDE THER_NON_LINE : PHASE DE PREDICTION
!
! ----------------------------------------------------------------------
!
!     VAR VTEMPM : ITERE PRECEDENT DU CHAMP DE TEMPERATURE
!     OUT VTEMPP : ITERE COURANT   DU CHAMP DE TEMPERATURE
!
!
    complex(kind=8) :: cbid
!
!
    integer :: k, j2nd,  j2ni
    integer ::  jtempm
    integer ::   jbuem
    real(kind=8) :: rbid
    character(len=1) :: typres
    character(len=19) :: chsol
    character(len=24) :: bidon, veresi, varesi, vabtla, vebtla, criter
    character(len=24) :: vebuem, vabuem, cnvabt, cnvabu
    integer :: iret
    real(kind=8), pointer :: btla(:) => null()
    real(kind=8), pointer :: dirp(:) => null()
    real(kind=8), pointer :: tempp(:) => null()
    real(kind=8), pointer :: temp(:) => null()
    real(kind=8), pointer :: vare(:) => null()
    cbid = dcmplx(0.d0, 0.d0)
!
    data typres        /'R'/
    data chsol         /'&&NXPRED.SOLUTION'/
    data bidon         /'&&FOMULT.BIDON'/
    data veresi        /'&&VERESI           .RELR'/
    data vebtla        /'&&VETBTL           .RELR'/
    data vabtla        /' '/
    data vebuem        /'&&VEBUEM           .RELR'/
    data vabuem        /' '/
    data cnresi        /' '/
!
! ----------------------------------------------------------------------
!
    call jemarq()
    varesi = '&&VARESI'
    criter = '&&RESGRA_GCPC'
    cnvabt = ' '
    cnvabu = ' '
!
! --- RECUPERATION D'ADRESSES
!
    call jeveuo(vec2nd(1:19)//'.VALE', 'L', j2nd)
    call jeveuo(vec2ni(1:19)//'.VALE', 'L', j2ni)
    call jeveuo(cndirp(1:19)//'.VALE', 'L', vr=dirp)
    call jeveuo(vtempp(1:19)//'.VALE', 'E', vr=tempp)
    call jeveuo(vtempm(1:19)//'.VALE', 'E', jtempm)
    call jeveuo(vtemp(1:19)//'.VALE', 'L', vr=temp)
!
    if (lostat) then
!
!=======================================================================
!  INITIALISATION POUR LE PREMIER PAS DE CALCUL
!=======================================================================
!
! --- VECTEURS RESIDUS ELEMENTAIRES - CALCUL ET ASSEMBLAGE
!
        call verstp(modele, charge, infoch, carele, mate,&
                    time, compor, vtemp, vtemp, vhydr,&
                    vhydrp, tmpchi, tmpchf, veresi)
        call asasve(veresi, numedd, typres, varesi)
        call ascova('D', varesi, bidon, 'INST', rbid,&
                    typres, cnresi)
        call jeveuo(cnresi(1:19)//'.VALE', 'L', vr=vare)
!
! --- BT LAMBDA - CALCUL ET ASSEMBLAGE
!
        call vethbt(modele, charge, infoch, carele, mate,&
                    vtemp, vebtla)
        call asasve(vebtla, numedd, typres, vabtla)
        call ascova('D', vabtla, bidon, 'INST', rbid,&
                    typres, cnvabt)
        call jeveuo(cnvabt(1:19)//'.VALE', 'L', vr=btla)
!
! --- B . TEMPERATURE - CALCUL ET ASSEMBLAGE
!
        call vethbu(modele, matass, charge, infoch, carele,&
                    mate, vtemp, vebuem)
        call asasve(vebuem, numedd, typres, vabuem)
        call ascova('D', vabuem, bidon, 'INST', rbid,&
                    typres, cnvabu)
        call jeveuo(cnvabu(1:19)//'.VALE', 'L', jbuem)
!
        do k = 1, lonch
            tempp(k) = zr(j2nd+k-1) - vare(k) + dirp(k) - btla(k)- zr(jb&
                             &uem+k-1)
        end do
!
! --- RESOLUTION (VTEMPP CONTIENT LE SECOND MEMBRE, CHSOL LA SOLUTION)
!
        call resoud(matass, maprec, solveu, cnchci, 0,&
                    vtempp, chsol, 'V', [0.d0], [cbid],&
                    criter, .true., 0, iret)
!
! --- RECOPIE DANS VTEMPM DU CHAMP SOLUTION CHSOL
!
        call copisd('CHAMP_GD', 'V', chsol, vtempm(1:19))
        call jeveuo(vtempm(1:19)//'.VALE', 'E', jtempm)
        do k = 1, lonch
            zr(jtempm+k-1) = zr(jtempm+k-1) + temp(k)
        end do
!
    else
!
!=======================================================================
!  INITIALISATION POUR LE PREMIER PAS, CALCUL TRANSITOIRE, PAS COURANT
!=======================================================================
!
        do k = 1, lonch
            tempp(k) = zr(j2ni+k-1) + dirp(k)
        end do
!
! --- RESOLUTION (VTEMPP CONTIENT LE SECOND MEMBRE, CHSOL LA SOLUTION)
!
        call resoud(matass, maprec, solveu, cnchci, 0,&
                    vtempp, chsol, 'V', [0.d0], [cbid],&
                    criter, .true., 0, iret)
!
! --- RECOPIE DANS VTEMPM DU CHAMP SOLUTION CHSOL
!
        call copisd('CHAMP_GD', 'V', chsol, vtempm(1:19))
!
    endif
!
    call jedema()
end subroutine
