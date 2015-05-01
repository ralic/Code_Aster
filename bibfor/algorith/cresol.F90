subroutine cresol(solveu)
    implicit none
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/crsvgc.h"
#include "asterfort/crsvld.h"
#include "asterfort/crsvmf.h"
#include "asterfort/crsvmu.h"
#include "asterfort/crsvpe.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/sdsolv.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=19) :: solveu
! ----------------------------------------------------------------------
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
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
!
!     CREATION D'UNE SD_SOLVEUR PAR LECTURE DU MOT CLE SOLVEUR
!
! IN/JXOUT K19 SOLVEU  : SD_SOLVEUR
!
! ----------------------------------------------------------------------
!
    integer :: zslvk, zslvr, zslvi
    integer :: istop, nsolve, ibid, nprec, islvk, islvr, islvi, n1
    real(kind=8) :: epsmat
    character(len=3) :: syme, mixpre, kmd, kellag
    character(len=8) :: kstop, modele, kxfem
    character(len=8) :: partit
    character(len=16) :: method, nomsol
    character(len=19) :: ligrmo
    integer :: eximc
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITS. GLOBALES (CAR MOT-CLES OPTIONNELS)
    nomsol='SOLVEUR'
    syme='NON'
    nprec=8
    istop=0
    kstop=' '
    epsmat=-1.d0
    mixpre='NON'
    kmd='NON'
    modele = ' '
    kellag='NON'
    kxfem=' '
!
    call getfac(nomsol, nsolve)
    if (nsolve .eq. 0) goto 10
    call getvtx(nomsol, 'METHODE', iocc=1, scal=method, nbret=ibid)
!
! ------------------------------------------------------
! --- LECTURE BLOC COMMUN A TOUS LES SOLVEURS LINEAIRES
! --- CES PARAMETRES NE SONT PAS FORCEMENT UTILISES PAR
! --- TOUS LES OPERATEURS ET TOUS LES SOLVEURS
! ------------------------------------------------------
!
! ----- STOP SINGULIER/NPREC
    eximc=getexm(nomsol,'STOP_SINGULIER')
    if (eximc .eq. 1) then
        call getvtx(nomsol, 'STOP_SINGULIER', iocc=1, scal=kstop, nbret=ibid)
    endif
    eximc=getexm(nomsol,'NPREC')
    if (eximc .eq. 1) then
        call getvis(nomsol, 'NPREC', iocc=1, scal=nprec, nbret=ibid)
        if (kstop .eq. 'OUI') then
            istop = 0
        else if (kstop.eq.'NON') then
            istop = 1
        endif
    endif
!
! ----- SYME
    eximc=getexm(nomsol,'SYME')
    if (eximc .eq. 1) then
        call getvtx(nomsol, 'SYME', iocc=1, scal=syme, nbret=ibid)
    endif
!
! ----- FILTRAGE_MATRICE
    eximc=getexm(nomsol,'FILTRAGE_MATRICE')
    if (eximc .eq. 1) then
        call getvr8(nomsol, 'FILTRAGE_MATRICE', iocc=1, scal=epsmat, nbret=ibid)
    endif
!
! ----- MIXER PRECISION
    eximc=getexm(nomsol,'MIXER_PRECISION')
    if (eximc .eq. 1) then
        call getvtx(nomsol, 'MIXER_PRECISION', iocc=1, scal=mixpre, nbret=ibid)
    endif
!
! ------ MATR_DISTRIBUEE
    eximc=getexm(nomsol,'MATR_DISTRIBUEE')
    if (eximc .eq. 1) then
        call getvtx(nomsol, 'MATR_DISTRIBUEE', iocc=1, scal=kmd, nbret=ibid)
        call getvid(' ', 'MODELE', scal=modele, nbret=ibid)
        ligrmo=modele//'.MODELE'
        call dismoi('PARTITION', ligrmo, 'LIGREL', repk=partit)
        if ((partit.eq.' ') .and. (kmd.eq.'OUI')) then
            kmd='NON'
            call utmess('I', 'ASSEMBLA_3')
        endif
    endif
!
! ------ ELIM_LAGR
    eximc=getexm(nomsol,'ELIM_LAGR')
    if (eximc .eq. 1) then
        call getvtx(nomsol, 'ELIM_LAGR', iocc=1, scal=kellag, nbret=n1)
        if (n1 .eq. 1) then
            if (kellag .ne. 'OUI') kellag='NON'
        else
            kellag='NON'
        endif
    endif
!
! ------ PRE_COND_XFEM
    eximc=getexm(' ','MODELE')
    if (eximc .eq. 1) then
        call getvid(' ', 'MODELE', scal=modele, nbret=n1)
            if (n1 .eq. 1 .and. modele .ne. ' ') then
               call dismoi('PRE_COND_XFEM', modele, 'MODELE', repk=kxfem)
            endif
    endif
!
    zslvk = sdsolv('ZSLVK')
    zslvr = sdsolv('ZSLVR')
    zslvi = sdsolv('ZSLVI')
    call wkvect(solveu//'.SLVK', 'V V K24', zslvk, islvk)
    call wkvect(solveu//'.SLVR', 'V V R', zslvr, islvr)
    call wkvect(solveu//'.SLVI', 'V V I', zslvi, islvi)
!
! ------------------------------------------------------
! --- LECTURE MOT-CLE ET REMPLISSAGE DE LA SD_SOLVEUR PROPRE A CHAQUE
!     SOLVEUR LINEAIRE
! ------------------------------------------------------
!
    if (method .eq. 'MUMPS') then
!     -----------------------------
        call crsvmu(nomsol, solveu, istop, nprec, syme,&
                    epsmat, mixpre, kmd, kellag, kxfem)
!
    else if (method.eq.'PETSC') then
!     -----------------------------
        call crsvpe(nomsol, solveu, istop, nprec, syme,&
                    epsmat, mixpre, kmd, kellag, kxfem)
!
    else if (method.eq.'LDLT') then
!     -----------------------------
        call crsvld(nomsol, solveu, istop, nprec, syme,&
                    epsmat, mixpre, kmd, kellag, kxfem)
!
    else if (method.eq.'GCPC') then
!     -----------------------------
        call crsvgc(nomsol, solveu, istop, nprec, syme,&
                    epsmat, mixpre, kmd, kellag, kxfem)
!
    else if (method.eq.'MULT_FRONT') then
!     -----------------------------
        call crsvmf(nomsol, solveu, istop, nprec, syme,&
                    epsmat, mixpre, kmd, kellag, kxfem)
!
    else
        ASSERT(.false.)
    endif
!
 10 continue
!
    call jedema()
end subroutine
