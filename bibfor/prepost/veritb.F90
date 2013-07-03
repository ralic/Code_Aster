subroutine veritb(nk1d, ndim, oridef)
!
    implicit     none
#include "asterc/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/tbexp2.h"
#include "asterfort/tbexv1.h"
#include "asterfort/u2mess.h"
#include "asterfort/verinr.h"
    integer :: nk1d, ndim
    character(len=8) :: oridef
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!
! ======================================================================
! ======================================================================
! --- BUT : VERIFICATION DE LA PRESENCE DES CHAMPS NECESSAIRES ---------
! ======================================================================
! IN  : NK1D   : NOMBRE D'OCCURENCE DE K1D -----------------------------
! --- : NDIM   : DIMENSION DE L'ESPACE ---------------------------------
! --- : ORIDEF : TYPE D'ORIENTATION DU DEFAUT --------------------------
! ======================================================================
    logical :: teste
    integer :: i, ibid, nbval1, nbval2
    character(len=8) :: motfac, k8b, tabrev, tabmdb, tabthr
    character(len=19) :: tbins1, tbins2
    integer :: iarg
! ======================================================================
    call jemarq()
! ======================================================================
! --- DEFINITION DES TABLES --------------------------------------------
! ======================================================================
    motfac = 'K1D'
    tbins1 = '&&VERITB.TBINS1'
    tbins2 = '&&VERITB.TBINS2'
! ======================================================================
! --- CAS DE LA PREMIERE OCCURENCE DE K1D ------------------------------
! ======================================================================
    call getvid(motfac, 'TABL_MECA_REV', 1, iarg, 1,&
                tabrev, ibid)
    call getvid(motfac, 'TABL_MECA_MDB', 1, iarg, 1,&
                tabmdb, ibid)
    call getvid(motfac, 'TABL_THER', 1, iarg, 1,&
                tabthr, ibid)
! ======================================================================
! --- VERIFICATION DE LA PRESENCE DE LISTE D'INSTANT -------------------
! ======================================================================
    call tbexp2(tabrev, 'INST')
    call tbexp2(tabmdb, 'INST')
    call tbexp2(tabthr, 'INST')
    call tbexp2(tabrev, 'ABSC_CURV')
    call tbexp2(tabthr, 'ABSC_CURV')
!
! ======================================================================
! --- VERIFICATION DE LA COHERENCE DES LISTES D'INSTANT POUR -----------
! --- LES CHAMPS MECANIQUES --------------------------------------------
! ======================================================================
    call tbexv1(tabrev, 'INST', tbins1, 'V', nbval1,&
                k8b)
    call tbexv1(tabmdb, 'INST', tbins2, 'V', nbval2,&
                k8b)
    if (nbval1 .ne. nbval2) then
        call u2mess('F', 'PREPOST4_90')
    endif
    teste = verinr ( nbval1, tbins1, tbins2 )
    if (teste) then
        call u2mess('F', 'PREPOST4_91')
    endif
! ======================================================================
! --- DESTRUCTIONS DES VECTEURS INUTILES -------------------------------
! ======================================================================
    call jedetr(tbins2)
    call jedetr(tabrev)
    call jedetr(tabmdb)
    call jedetr(tabthr)
! ======================================================================
! --- ITERATIONS SUR LES OCCURENCES DE K1D -----------------------------
! ======================================================================
    do 10 i = 2, nk1d
! ======================================================================
! --- RECUPERATION DES TABLES ASSOCIEES A K1D POUR L'ITERATION COURANTE-
! ======================================================================
        call getvid(motfac, 'TABL_MECA_REV', i, iarg, 1,&
                    tabrev, ibid)
        call getvid(motfac, 'TABL_MECA_MDB', i, iarg, 1,&
                    tabmdb, ibid)
        call getvid(motfac, 'TABL_THER', i, iarg, 1,&
                    tabthr, ibid)
! ======================================================================
! --- VERIFICATION DE LA PRESENCE DE LISTE D'INSTANT -------------------
! ======================================================================
        call tbexp2(tabrev, 'INST')
        call tbexp2(tabmdb, 'INST')
        call tbexp2(tabthr, 'INST')
! ======================================================================
! --- VERIFICATION DE LA COHERENCE DES LISTES D'INSTANT POUR -----------
! --- LES CHAMPS MECANIQUES --------------------------------------------
! ======================================================================
        call tbexv1(tabrev, 'INST', tbins2, 'V', nbval2,&
                    k8b)
        if (nbval1 .ne. nbval2) then
            call u2mess('F', 'PREPOST4_92')
        endif
        teste = verinr ( nbval1, tbins1, tbins2)
        if (teste) then
            call u2mess('F', 'PREPOST4_91')
        endif
        call jedetr(tbins2)
        call tbexv1(tabmdb, 'INST', tbins2, 'V', nbval2,&
                    k8b)
        if (nbval1 .ne. nbval2) then
            call u2mess('F', 'PREPOST4_92')
        endif
        teste = verinr ( nbval1, tbins1, tbins2)
        if (teste) then
            call u2mess('F', 'PREPOST4_91')
        endif
        call jedetr(tbins2)
        call jedetr(tabrev)
        call jedetr(tabmdb)
        call jedetr(tabthr)
10  end do
    call jedetr(tbins1)
! ======================================================================
! --- VERIFICATION DE LA PRESENCE DES BONNES COMPOSANTES POUR LE -------
! --- CALCUL DES FACTEURS D'INTENSITE DE CONTRAINTE --------------------
! ======================================================================
    do 20 i = 1, nk1d
! ======================================================================
! --- RECUPERATION DES TABLES ASSOCIEES A K1D POUR L'ITERATION COURANTE-
! ======================================================================
        call getvid(motfac, 'TABL_MECA_REV', i, iarg, 1,&
                    tabrev, ibid)
        call getvid(motfac, 'TABL_MECA_MDB', i, iarg, 1,&
                    tabmdb, ibid)
        call getvid(motfac, 'TABL_THER', i, iarg, 1,&
                    tabthr, ibid)
        if (ndim .eq. 2) then
! ======================================================================
! --- CAS D'UNE DIMENSION D'ORDRE 2 ------------------------------------
! ======================================================================
            if (oridef .eq. 'CIRC') then
! ======================================================================
! --- CAS D'UN DEFAUT CIRCONFERENTIEL ----------------------------------
! ======================================================================
                call tbexp2(tabrev, 'SIYY')
                call tbexp2(tabmdb, 'SIYY')
            else
! ======================================================================
! --- CAS D'UN DEFAUT LONGITUDINAL -------------------------------------
! ======================================================================
                call tbexp2(tabrev, 'SIZZ')
                call tbexp2(tabmdb, 'SIZZ')
            endif
        else
! ======================================================================
! --- CAS D'UNE DIMENSION D'ORDRE 3 ------------------------------------
! ======================================================================
            if (oridef .eq. 'CIRC') then
! ======================================================================
! --- CAS D'UN DEFAUT CIRCONFERENTIEL ----------------------------------
! ======================================================================
                call tbexp2(tabrev, 'SIZZ')
                call tbexp2(tabmdb, 'SIZZ')
            else
! ======================================================================
! --- CAS D'UN DEFAUT LONGITUDINAL -------------------------------------
! ======================================================================
                call tbexp2(tabrev, 'SIXX')
                call tbexp2(tabmdb, 'SIXX')
                call tbexp2(tabrev, 'SIYY')
                call tbexp2(tabmdb, 'SIYY')
                call tbexp2(tabrev, 'SIZZ')
                call tbexp2(tabmdb, 'SIZZ')
                call tbexp2(tabrev, 'SIXY')
                call tbexp2(tabmdb, 'SIXY')
                call tbexp2(tabrev, 'COOR_X')
                call tbexp2(tabrev, 'COOR_Y')
            endif
        endif
! ======================================================================
! --- VERIFICATION DU PARAMETRE TEMP POUR LA TABLE DONNEE THERMIQUE ----
! ======================================================================
        call tbexp2(tabthr, 'TEMP')
        call jedetr(tabrev)
        call jedetr(tabmdb)
        call jedetr(tabthr)
20  end do
! ======================================================================
    call jedema()
! ======================================================================
end subroutine
