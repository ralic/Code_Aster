subroutine nxnewt(modele, mate, carele, charge, infcha,&
                  infoch, numedd, solveu, time, lonch,&
                  matass, maprec, cnchci, vtemp, vtempm,&
                  vtempp, vec2nd, mediri, conver, vhydr,&
                  vhydrp, tmpchi, tmpchf, compor, cnvabt,&
                  cnresi, parcri, parcrr, reasma, testr,&
                  testm)
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
#include "asterfort/asmatr.h"
#include "asterfort/copisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/merxth.h"
#include "asterfort/preres.h"
#include "asterfort/resoud.h"
#include "asterfort/verstp.h"
#include "asterfort/vethbt.h"
    integer :: lonch
    logical :: conver, reasma
    character(len=19) :: infcha, solveu, maprec
    character(len=24) :: modele, mate, carele, charge, infoch, numedd, time
    character(len=24) :: matass, cnchci, cnresi, vtemp, vtempm, vtempp, vec2nd
    character(len=24) :: vhydr, vhydrp, compor, tmpchi, tmpchf
    integer :: parcri(3)
    real(kind=8) :: parcrr(2)
!
! ----------------------------------------------------------------------
!
! COMMANDE THER_NON_LINE : ITERATION DE NEWTON
!
! ----------------------------------------------------------------------
!
!     VAR VTEMPM : ITERE PRECEDENT DU CHAMP DE TEMPERATURE
!     OUT VTEMPP : ITERE COURANT   DU CHAMP DE TEMPERATURE
!     OUT VHYDRP : ITERE COURANT   DU CHAMP D HYDRATATION
!
!
    complex(kind=8) :: cbid
!
!
    integer :: k, jvare, j2nd, jtempp, ibid
    integer :: jbtla, jmed, jmer, nbmat, ierr
    real(kind=8) :: r8bid
    character(len=1) :: typres
    character(len=19) :: chsol
    character(len=24) :: bidon, veresi, varesi, vabtla, vebtla, criter
    character(len=24) :: tlimat(2), mediri, merigi, cnvabt
    real(kind=8) :: testr, testm, vnorm, rbid
    integer :: iret
!
    data typres        /'R'/
    data chsol         /'&&NXNEWT.SOLUTION'/
    data bidon         /'&&FOMULT.BIDON'/
    data veresi        /'&&VERESI           .RELR'/
    data vebtla        /'&&VETBTL           .RELR'/
    data merigi        /'&&METRIG           .RELR'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
    varesi = '&&VARESI'
    vabtla = '&&VATBTL'
    cnresi = ' '
    cnvabt = ' '
    criter = '&&RESGRA_GCPC'
!
! --- RECUPERATION D'ADRESSES
!
    call jeveuo(vec2nd(1:19)//'.VALE', 'L', j2nd)
!
! --- VECTEURS RESIDUS ELEMENTAIRES - CALCUL ET ASSEMBLAGE
!
    call verstp(modele, charge, infoch, carele, mate,&
                time, compor, vtemp, vtempm, vhydr,&
                vhydrp, tmpchi, tmpchf, veresi)
    call asasve(veresi, numedd, typres, varesi)
    call ascova('D', varesi, bidon, 'INST', r8bid,&
                typres, cnresi)
    call jeveuo(cnresi(1:19)//'.VALE', 'L', jvare)
!
! --- BT LAMBDA - CALCUL ET ASSEMBLAGE
!
    call vethbt(modele, charge, infoch, carele, mate,&
                vtempm, vebtla)
    call asasve(vebtla, numedd, typres, vabtla)
    call ascova('D', vabtla, bidon, 'INST', r8bid,&
                typres, cnvabt)
    call jeveuo(cnvabt(1:19)//'.VALE', 'L', jbtla)
!
!==========================================================
! --- CALCUL DU RESIDU ET
!     DU CRITERE DE CONVERGENCE DES ITERATIONS (NORME SUP)
!==========================================================
!
    call jeveuo(vtempp(1:19)//'.VALE', 'E', jtempp)
    testr = 0.d0
    testm = 0.d0
    vnorm = 0.d0
    do 100 k = 1, lonch
        zr(jtempp+k-1) = zr(j2nd+k-1) - zr(jvare+k-1) - zr(jbtla+k-1)
        testr = testr + ( zr(jtempp+k-1) )**2
        vnorm = vnorm + ( zr(j2nd+k-1) - zr(jbtla+k-1) )**2
        testm = max( testm,abs( zr(jtempp+k-1) ) )
100  end do
    if (vnorm .gt. 0d0) then
        testr = sqrt( testr / vnorm )
    endif
!
    if (parcri(1) .ne. 0) then
        if (testm .lt. parcrr(1)) then
            conver=.true.
            call copisd('CHAMP_GD', 'V', vtempm(1:19), vtempp(1:19))
            goto 999
        else
            conver=.false.
        endif
    else
        if (testr .lt. parcrr(2)) then
            conver=.true.
            call copisd('CHAMP_GD', 'V', vtempm(1:19), vtempp(1:19))
            goto 999
        else
            conver=.false.
        endif
    endif
!
    if (reasma) then
!
!==========================================================
! --- (RE)CALCUL DE LA MATRICE TANGENTE
!==========================================================
!
        call merxth(modele, charge, infoch, carele, mate,&
                    time, vtempm, merigi, compor, tmpchi,&
                    tmpchf)
        call jeveuo(merigi, 'L', jmer)
        call jeveuo(mediri, 'L', jmed)
!
        nbmat = 0
        if (zk24(jmer)(1:8) .ne. '        ') then
            nbmat = nbmat + 1
            tlimat(nbmat) =merigi(1:19)
        endif
        if (zk24(jmed)(1:8) .ne. '        ') then
            nbmat = nbmat + 1
            tlimat(nbmat) =mediri(1:19)
        endif
!
! --- ASSEMBLAGE DE LA MATRICE
!
        call asmatr(nbmat, tlimat, ' ', numedd, solveu,&
                    infcha, 'ZERO', 'V', 1, matass)
!
! --- DECOMPOSITION OU CALCUL DE LA MATRICE DE PRECONDITIONNEMENT
!
        call preres(solveu, 'V', ierr, maprec, matass,&
                    ibid, -9999)
!
    endif
!
!==========================================================
! --- RESOLUTION (VTEMPP CONTIENT LE SECOND MEMBRE, CHSOL LA SOLUTION)
!
    call resoud(matass, maprec, solveu, cnchci, 0,&
                vtempp, chsol, 'V', [0.d0], [cbid],&
                criter, .true., 0, iret)
!
! --- RECOPIE DANS VTEMPP DU CHAMP SOLUTION CHSOL,
!     INCREMENT DE TEMPERATURE
!
    call copisd('CHAMP_GD', 'V', chsol, vtempp(1:19))
!
999  continue
    call jedema()
end subroutine
