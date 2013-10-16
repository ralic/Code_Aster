subroutine op0116()
    implicit none
#include "asterfort/utmess.h"
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
! person_in_charge: hassan.berro@edf.fr
! OPERATEUR CALCULANT LE CHAMP DE PRESSION DANS UN FLUIDE SELON LES
! HYPOTHESES D'UN ECOULEMENT POTENTIEL
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8pi.h"
#include "asterc/r8prem.h"
#include "asterc/getres.h"
#include "asterfort/alimrs.h"
#include "asterfort/calflu.h"
#include "asterfort/chpver.h"
#include "asterfort/chflch.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ntarch.h"
#include "asterfort/ntetcr.h"
#include "asterfort/nxnoli.h"
#include "asterfort/prstoc.h"
#include "asterfort/rcmfmc.h"
#include "asterfort/resoud.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/tabcor.h"
#include "asterfort/vtcmbl.h"
#include "asterfort/wkvect.h"
!
    aster_logical :: ltrans, lbid
    integer :: i, ib, imod,  nbmod, j1, jdisc, dim, icor(2), iadx, iady 
    integer :: nbdesc, nbrefe, nbvale, nbcoefs, icoef, jcoef
    real(kind=8) :: r8b, pi, freq, para(2), constant,  epstol
    character(len=2) :: modelisa(2), modelis
    character(len=8) :: mater, rigthe, modmec, nomres, potentiel
    character(len=8) :: mailla, maflui, nume
    character(len=19) :: k19b, chamno, vectas, solve1, solve2, resuas, nomch(3)
    character(len=19) :: tmpmod
    character(len=24) :: typres, nomcom, mate, metres, sdieto, arcinf, inflis
    character(len=24) :: chcmb2, vesolx, vesoly, vesolz, vect2
    character(len=24) :: liditr, nuddl, modele, bl24, chcomb, chamnx, chamny, chamnz
    complex(kind=8) :: c16b
    modele  = ' '
    mater   = ' '
    mate    = ' '
    rigthe  = ' '
    modmec  = ' '
    chamno  = ' '
    bl24    = ' '
    para(1) = 1.0
    para(2) = -1.d150
    pi = r8pi()
    epstol = 1.d5*r8prem()
    r8b=0.d0
    c16b = dcmplx(0.,0.)
!---------------------------------------------------------------------
    call jemarq()
!---------------------------------------------------------------------
    call getres(nomres, typres, nomcom)
    
!---------------------------------------------------------------------
!---------- RECUPERATION DES ARGUMENTS DE LA COMMANDE ----------------
!---------------------------------------------------------------------
    call getvtx(' ', 'POTENTIEL' , iocc=1, scal=potentiel, nbret=ib)
    call getvid(' ', 'RIGI_THER',  iocc=1, scal=rigthe, nbret=ib)
    call getvid(' ', 'MODE_MECA',  iocc=1, scal=modmec, nbret=imod)
!---------------------------------------------------------------------
!---------- MODELE THERMIQUE ET CHAMP DE MATERIAUX -------------------
!---------------------------------------------------------------------
    call dismoi('NOM_MODELE', rigthe, 'MATR_ASSE', repk=modele)
    call dismoi('CHAM_MATER', rigthe, 'MATR_ASSE', repk=mater )
    if (ib .ne. 0) call rcmfmc(mater, mate)
!---------------------------------------------------------------------
!---------- NUMEROTATION DES DDL FLUIDES -----------------------------
!---------------------------------------------------------------------
    call dismoi('NOM_NUME_DDL', rigthe, 'MATR_ASSE', repk=nuddl)

!---------------------------------------------------------------------
!---------- PARAMETRES DU SOLVEUR ------------------------------------
!---------------------------------------------------------------------
    call dismoi('SOLVEUR', rigthe, 'MATR_ASSE', repk=solve1)
    call jeveuo(solve1//'.SLVK', 'E', j1)
    metres=zk24(j1)
    if (metres .ne. 'MUMPS' .and. metres .ne. 'PETSC') then
        solve2='&&OP0116.SOLVEUR'
        call copisd('SOLVEUR', 'V', solve1, solve2)
    else
        solve2=solve1
    endif

    call dismoi('DIM_GEOM', modele, 'MODELE', repi=dim)
    call dismoi('NOM_MAILLA', modele, 'MODELE', repk=maflui)

    modelisa = ['2D' , '3D']
    modelis = modelisa(dim-1)

    vect2 = '&&OP0116.2ND.MEMBRE'
    call chflch(rigthe,vect2)


!---------------------------------------------------------------------
!---------- APPEL A CALFLU -------------------------------------------
!---------------------------------------------------------------------
    if (imod .ne. 0) then
        call dismoi('NUME_DDL', modmec, 'RESU_DYNA', repk=nume)
        call dismoi('NOM_MAILLA', nume, 'NUME_DDL', repk=mailla)
        call dismoi('NB_CHAMPS', modmec, 'RESU_DYNA', repi=nbmod)
        call wkvect('&&OP0116.TXSTO', 'V V K24', nbmod, iadx)
        call wkvect('&&OP0116.TYSTO', 'V V K24', nbmod, iady)
        call wkvect('&&OP0116.TZSTO', 'V V K24', nbmod, iady)

        call tabcor(modelis, mate, mailla, maflui, modele, nuddl, 0, icor)

        call getvr8(' ', 'COEF_MULT',  iocc=1, nbval=0, nbret=icoef)
        nbcoefs = 1
        if (icoef .ne. 0) nbcoefs = -icoef

        call wkvect('&&OP0116.COEFSMOD', 'V V R', nbcoefs, jcoef)
        call getvr8(' ', 'COEF_MULT',  iocc=1, nbval=nbcoefs, vect=zr(jcoef))

        do 10 i = 1, nbmod
            call rsexch('F', modmec, 'DEPL', i, chamno, ib)
            call rsadpa(modmec, 'L', 1, 'FREQ', i, 0, sjv=j1)
            freq = zr(j1)
!
            constant = 1
            if (i .le. nbcoefs) then
                constant = zr(jcoef+i-1)
            endif
!
            tmpmod = '&&OP0116.MODxMOIN.1'
            if (potentiel(1:5) .eq. 'PRESS') then
                constant = constant * 4.d0*pi*pi*freq*freq
            elseif (potentiel(1:4) .eq. 'VITE') then
                constant = constant * 2.d0*pi*freq
            elseif (potentiel(1:4) .eq. 'DEPL') then
                constant = constant * 1.0
            endif
!
            call vtcmbl(1, 'R', [constant], 'R', [chamno], 'R', tmpmod)
!
            chamnx = '&&OP0116.CHAMNX'
            chamny = '&&OP0116.CHAMNY'
            vesolx = '&&OP0116.VESOLX'
            vesoly = '&&OP0116.VESOLY'
            call alimrs(mate, mailla, maflui, modele, 0,nuddl, tmpmod, chamnx, 'DX', icor)
            call alimrs(mate, mailla, maflui, modele, 0,nuddl, tmpmod, chamny, 'DY', icor)
            call calflu(chamnx, modele, mate, nuddl, vesolx, nbdesc, nbrefe, nbvale, 'X')
            call calflu(chamny, modele, mate, nuddl, vesoly, nbdesc, nbrefe, nbvale, 'Y')
!
            chcomb = "&&OP0116.CHCOMB"
            nomch  = [vect2(1:19), vesolx(1:19),vesoly(1:19)]
            call vtcmbl(3, ['R','R','R'], [1.d0, 1.d0, 1.d0], ['R','R','R'], nomch,'R', chcomb)
!
            if (dim .eq. 3) then
                chamnz = '&&OP0116.CHAMNZ'
                vesolz = '&&OP0116.VESOLZ'
                call alimrs(mate, mailla, maflui, modele, 0,nuddl, tmpmod, chamnz, 'DZ', icor)
                call calflu(chamnz, modele, mate, nuddl, vesolz, nbdesc, nbrefe, nbvale, 'Z')
!
                chcmb2 = "&&OP0116.CHCOMB2"
                call vtcmbl(2, ['R','R'], [1.d0, 1.d0], ['R','R'], [chcomb,vesolz], 'R', chcmb2)
                chcomb = chcmb2
            endif
!
            vectas = chcomb(1:19)
            resuas = '&&NXLECTVAR_____'
            call chpver('F', vectas, 'NOEU', '*', ib)
            call resoud(rigthe, ' ', solve2, ' ', 0,&
                        vectas, resuas, 'V', [r8b], [c16b],&
                        ' ', .true._1, 0, ib)
!
            if (i .eq. 1) then
                ltrans = .false.
                k19b = '                   '
                sdieto = '&&OP0116.SDIETO'
                call ntetcr(nuddl, .false._1, sdieto)
!
                inflis = '&&OP0116.SDDIS.ARCH.INFL'
                call wkvect(inflis, 'V V R', 3, j1)
                zr(j1) = 1.0
                zr(j1+1) = 1.0
                zr(j1+2) = nbmod
!
                arcinf = '&&OP0116.SDDIS.ARCH.AINF'
                call wkvect(arcinf, 'V V I', nbmod, j1)
                zi(j1) = 0
!
                liditr = '&&OP0116.SDDIS.ARCH.DITR'
                call wkvect(liditr, 'V V R', nbmod, jdisc)
                zr(jdisc+1-1) = freq
!
                call nxnoli(modele, mate, bl24, .false._1, .false._1,&
                            .false._1, ltrans, para, arcinf(1:19), k19b,&
                            sdieto, k19b)
            endif
            
            k19b = '                   '
!           --- Pour les modes doubles, assurer la qualite monotone de la discretisation
!               en frequence (indispensable pour un stockage evol_ther)
            if (i.gt.1) then
                if (freq.le. zr(jdisc+i-2)) then
                    freq = zr(jdisc+i-2) + epstol
                end if
            end if
            zr(jdisc+i-1) = freq

            call ntarch(i-1, modele, mate, bl24, .false._1,&
                        para, arcinf(1:19), k19b, sdieto, k19b,&
                        lbid)
!
10      continue
    endif
!
!
    call jedema()
end subroutine
