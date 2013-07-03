subroutine lisccm(nomcmd, lischa)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit     none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lisico.h"
#include "asterfort/lislch.h"
#include "asterfort/lislco.h"
#include "asterfort/lisnnb.h"
#include "asterfort/u2mesk.h"
    character(len=16) :: nomcmd
    character(len=19) :: lischa
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! VERIFICATION COMPATIBILITE CHARGE/COMMANDE
!
! ----------------------------------------------------------------------
!
!
! IN  NOMCMD : NOM DE LA COMMANDE
!              ' ' SI PAS DE COMMANDE
! IN  LISCHA : SD LISTE DES CHARGES
!
!
!
!
    logical :: lelim, ldual, lneum, lsigi, lvitf, limpf
    logical :: levoc, linte, londf, londp
    logical :: lveac, lveas, lveag
    logical :: lcomp
    integer :: ichar, nbchar
    integer :: codcha
    character(len=24) :: valk(2)
    character(len=8) :: charge
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    if (nomcmd .eq. ' ') goto 999
!
! --- NOMBRE DE CHARGES
!
    call lisnnb(lischa, nbchar)
    if (nbchar .eq. 0) goto 999
!
    do 10 ichar = 1, nbchar
!
! ----- CODE DU GENRE DE LA CHARGE
!
        call lislco(lischa, ichar, codcha)
        call lislch(lischa, ichar, charge)
        valk(1) = charge
        valk(2) = nomcmd
!
! ----- IDENTIFICATION DES GENRES ACTIFS DANS LA CHARGE
!
        lelim = lisico('DIRI_ELIM' ,codcha)
        ldual = lisico('DIRI_DUAL' ,codcha)
        lneum = lisico('NEUM_MECA' ,codcha)
        lsigi = lisico('SIGM_INTERNE' ,codcha)
        lvitf = lisico('VITE_FACE' ,codcha)
        limpf = lisico('IMPE_FACE' ,codcha)
        levoc = lisico('EVOL_CHAR' ,codcha)
        linte = lisico('INTE_ELEC' ,codcha)
        londf = lisico('ONDE_FLUI' ,codcha)
        londp = lisico('ONDE_PLANE' ,codcha)
        lveac = lisico('VECT_ASSE_CHAR',codcha)
        lveag = lisico('VECT_ASSE_GENE',codcha)
        lveas = lisico('VECT_ASSE' ,codcha)
!
! ----- VERIFICATION COMPATIBILITE DIRI_DUAL
!
        if (ldual) then
            lcomp = .false.
            if ((nomcmd.eq.'DYNA_LINE_HARM') .or. ( nomcmd.eq.'STAT_NON_LINE') .or.&
                (nomcmd.eq.'DYNA_NON_LINE') .or. (nomcmd.eq.'MECA_STATIQUE')) lcomp = .true.
            if (.not.lcomp) call u2mesk('F', 'CHARGES5_3', 2, valk)
        endif
!
! ----- VERIFICATION COMPATIBILITE DIRI_ELIM
!
!
        if (lelim) then
            lcomp = .false.
            if ((nomcmd.eq.'STAT_NON_LINE') .or. ( nomcmd.eq.'DYNA_NON_LINE') .or.&
                (nomcmd.eq.'MECA_STATIQUE')) lcomp = .true.
            if (.not.lcomp) call u2mesk('F', 'CHARGES5_3', 2, valk)
        endif
!
! ----- VERIFICATION COMPATIBILITE NEUM_MECA
!
        if (lneum) then
            lcomp = .false.
            if ((nomcmd.eq.'DYNA_LINE_TRAN') .or. ( nomcmd.eq.'DYNA_LINE_HARM') .or.&
                ( nomcmd.eq.'MECA_STATIQUE') .or. (nomcmd.eq.'STAT_NON_LINE') .or.&
                (nomcmd.eq.'DYNA_NON_LINE') .or. ( nomcmd.eq.'CALC_CHAMP')) lcomp = .true.
            if (.not.lcomp) call u2mesk('F', 'CHARGES5_3', 1, valk)
        endif
!
! ----- VERIFICATION COMPATIBILITE SIGM_INTERNE
!
        if (lsigi) then
            lcomp = .false.
            if ((nomcmd.eq.'DEFI_CABLE_BP') .or. ( nomcmd.eq.'STAT_NON_LINE') .or.&
                (nomcmd.eq.'DYNA_NON_LINE')) lcomp = .true.
            if (.not.lcomp) call u2mesk('F', 'CHARGES5_3', 1, valk)
        endif
!
! ----- VERIFICATION COMPATIBILITE VITE_FACE
!
        if (lvitf) then
            lcomp = .false.
            if ((nomcmd.eq.'CALC_VECT_ELEM') .or. ( nomcmd.eq.'MACR_ELEM_STAT')) lcomp = &
                                                                                 .true.
            if (.not.lcomp) call u2mesk('F', 'CHARGES5_3', 1, valk)
        endif
!
! ----- VERIFICATION COMPATIBILITE IMPE_FACE
!
        if (limpf) then
            lcomp = .false.
            if (nomcmd .eq. 'MACR_ELEM_STAT') lcomp = .true.
            if (.not.lcomp) call u2mesk('F', 'CHARGES5_3', 1, valk)
        endif
!
! ----- VERIFICATION COMPATIBILITE EVOL_CHAR
!
        if (levoc) then
            lcomp = .false.
            if ((nomcmd.eq.'CALC_VECT_ELEM') .or. ( nomcmd.eq.'MACR_ELEM_STAT') .or.&
                ( nomcmd.eq.'STAT_NON_LINE') .or. (nomcmd.eq.'DYNA_NON_LINE') .or.&
                (nomcmd.eq.'CALC_CHAMP')) lcomp = .true.
            if (.not.lcomp) call u2mesk('F', 'CHARGES5_3', 1, valk)
        endif
!
! ----- VERIFICATION COMPATIBILITE INTE_ELEC
!
        if (linte) then
            lcomp = .false.
            if ((nomcmd.eq.'CALC_VECT_ELEM') .or. ( nomcmd.eq.'MACR_ELEM_STAT') .or.&
                ( nomcmd.eq.'STAT_NON_LINE') .or. (nomcmd.eq.'DYNA_NON_LINE') .or.&
                (nomcmd.eq.'MECA_STATIQUE')) lcomp = .true.
            if (.not.lcomp) call u2mesk('F', 'CHARGES5_3', 1, valk)
        endif
!
! ----- VERIFICATION COMPATIBILITE ONDE_FLUIDE
!
        if (londf) then
            lcomp = .false.
            if ((nomcmd.eq.'CALC_VECT_ELEM') .or. ( nomcmd.eq.'MACR_ELEM_STAT') .or.&
                ( nomcmd.eq.'CALC_MATR_ELEM')) lcomp = .true.
            if (.not.lcomp) call u2mesk('F', 'CHARGES5_3', 1, valk)
        endif
!
! ----- VERIFICATION COMPATIBILITE ONDE_PLANE
!
        if (londp) then
            lcomp = .false.
            if ((nomcmd.eq.'STAT_NON_LINE') .or. ( nomcmd.eq.'DYNA_NON_LINE') .or.&
                ( nomcmd.eq.'DYNA_LINE_TRAN')) lcomp = .true.
            if (.not.lcomp) call u2mesk('F', 'CHARGES5_3', 1, valk)
        endif
!
! ----- VERIFICATION COMPATIBILITE VECT_ASSE_CHAR
!
        if (lveac) then
            lcomp = .false.
            if ((nomcmd.eq.'DYNA_LINE_TRAN') .or. ( nomcmd.eq.'DYNA_LINE_HARM') .or.&
                ( nomcmd.eq.'MECA_STATIQUE') .or. (nomcmd.eq.'STAT_NON_LINE') .or.&
                (nomcmd.eq.'DYNA_NON_LINE') .or. ( nomcmd.eq.'CALC_CHAMP')) lcomp = .true.
            if (.not.lcomp) call u2mesk('F', 'CHARGES5_3', 1, valk)
        endif
!
! ----- VERIFICATION COMPATIBILITE VECT_ASSE
!
        if (lveas) then
            lcomp = .false.
            if ((nomcmd.eq.'DYNA_LINE_HARM')) lcomp = .true.
            if (.not.lcomp) call u2mesk('F', 'CHARGES5_3', 1, valk)
        endif
!
! ----- VERIFICATION COMPATIBILITE VECT_ASSE_GENE
!
        if (lveag) then
            lcomp = .false.
            if ((nomcmd.eq.'DYNA_LINE_HARM')) lcomp = .true.
            if (.not.lcomp) call u2mesk('F', 'CHARGES5_3', 1, valk)
        endif
!
10  end do
!
999  continue
!
    call jedema()
end subroutine
