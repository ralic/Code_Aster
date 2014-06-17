subroutine orth99(nomres, ritz)
    implicit none
#include "jeveux.h"
#include "asterc/gettco.h"
#include "asterfort/copmod.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtdscr.h"
#include "asterfort/refdcp.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rsorac.h"
#include "asterfort/utmess.h"
#include "asterfort/vecind.h"
#include "asterfort/vpgskp.h"
#include "asterfort/vtcrem.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=8) :: nomres
    integer :: ritz
!----------------------------------------------------------------------
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
!----------------------------------------------------------------------
!
!         DEFI_BASE_MODALE : ORTHO_BASE
!
! CE MOT CLE PERMET D'ORTHONORMALISER UNE BASE MODALE QUELCONQUE
!
!----------------------------------------------------------------------
!
!
    integer :: ifm, niv, n1, ier, ibid, imatra, nbmode, jordm, iddeeq
    integer ::  neq, idmode,    iorol, iorne, iad
    integer :: jiad,  ieq, i, nindep, ir, tmod(1)
    real(kind=8) :: alpha, rbid
    complex(kind=8) :: cbid
    character(len=8) :: k8b, matras, base, ortho, intf
    character(len=16) :: typbas
    character(len=14) :: nu, numdd1, numdda, matri1
    character(len=19) :: matr, chamol
    real(kind=8), pointer :: trav1(:) => null()
    real(kind=8), pointer :: trav3(:) => null()
    integer, pointer :: trav4(:) => null()
    integer, pointer :: nequ(:) => null()
    real(kind=8), pointer :: vale(:) => null()
!----------------------------------------------------------------------
    call jemarq()
!
    alpha = 0.717d0
!
!     ---RECUPERATION DU NIVEAU D'IMPRESSION---
!
    call infmaj()
    call infniv(ifm, niv)
!
!----------------------------------------------------------------------
! --- RECUPERATION DE LA MATRICE ASSEMBLEE
!-----------------------------------------------------------------------
!
    if (ritz .eq. 1) then
!
        call getvtx('  ', 'ORTHO', iocc=1, nbval=8, vect=ortho,&
                    nbret=ibid)
!
        if (ortho .eq. 'OUI     ') then
            call getvid(' ', 'MATRICE', scal=matras, nbret=n1)
        else
            goto 999
        endif
    else
        call getvid('ORTHO_BASE', 'MATRICE', iocc=1, scal=matras, nbret=n1)
    endif
!
    if (n1 .ne. 0) then
        call dismoi('NOM_NUME_DDL', matras, 'MATR_ASSE', repk=numdda)
        call mtdscr(matras)
        matr=matras
        call jeveuo(matr//'.&INT', 'E', imatra)
        call dismoi('NOM_NUME_DDL', matras, 'MATR_ASSE', repk=numdda)
    else
        matr=' '
    endif
!
!----------------------------------------------------------------------
! --- RECUPERATION DES MODES PROPRES
!-----------------------------------------------------------------------
!
    if (ritz .eq. 1) then
        base=nomres
    else
        call getvid('ORTHO_BASE', 'BASE', iocc=1, scal=base, nbret=n1)
    endif
!
! RECUPERATION DU TYPE ET DU NBRE DE MODES DES BASES
    call gettco(base, typbas)
    call rsorac(base, 'LONUTI', 0, rbid, k8b,&
                cbid, rbid, 'ABSOLU', tmod, 1,&
                ibid)
    nbmode=tmod(1)
!
!
    call jeveuo(base//'           .ORDR', 'L', jordm)
! RECUPERATION DE LA NUMEROTATION DES BASES
    if ((typbas.eq.'MODE_MECA') .or. (typbas.eq.'MODE_GENE')) then
        call dismoi('REF_RIGI_PREM', base, 'RESU_DYNA', repk=matri1)
    else
        call dismoi('REF_AMOR_PREM', base, 'RESU_DYNA', repk=matri1)
    endif
    if (matri1 .ne. ' ') then
        call dismoi('NOM_NUME_DDL', matri1, 'MATR_ASSE', repk=numdd1)
    else
        call dismoi('NUME_DDL', base, 'RESU_DYNA', repk=numdd1)
    endif
!
    call dismoi('REF_INTD_PREM', base, 'RESU_DYNA', repk=intf, arret='C',&
                ier=ir)
!
    if (numdd1 .ne. numdda) then
        call utmess('I', 'ALGELINE2_81')
    endif
    nu = numdda(1:14)
    call jeveuo(nu//'.NUME.DEEQ', 'L', iddeeq)
    call jeveuo(nu//'.NUME.NEQU', 'L', vi=nequ)
    neq = nequ(1)
    call wkvect('&&ORTH99.BASE', 'V V R', nbmode*neq, idmode)
    call copmod(base, numer=nu, bmodr=zr(idmode))
!-- FINALEMENT SI, DONC RECOPIE OK
!
!-----------------------------------------------------------------------
    AS_ALLOCATE(vr=trav1, size=neq)
    AS_ALLOCATE(vr=trav3, size=nbmode)
    AS_ALLOCATE(vi=trav4, size=neq)
!
    do i = 1, neq
        trav4(i) = 1
    end do
!
    if (matr .eq. ' ') then
! ORTHONORMALISATION L2
        call vpgskp(neq, nbmode, zr(idmode), alpha, imatra,&
                    0, trav1, trav4,trav3)
    else
! ORTHONORMALISATION PAR RAPPORT A LA MATRICE
        call vpgskp(neq, nbmode, zr(idmode), alpha, imatra,&
                    2, trav1, trav4,trav3)
    endif
! MISE A ZEROS DES VECTEURS NON INDEPENDANTS
    call vecind(matr, idmode, neq, nbmode, 0,&
                nindep)
!
!-- GESTION DES CONCEPTS REENTRANTS
    call jeexin(nomres//'           .DESC', ier)
    if (ier .ne. 0) then
        call wkvect('&&ORTH99.VECT_TEMP', 'V V I', nbmode, ibid)
        do i = 1, nbmode
            zi(ibid+i-1)=zi(jordm+i-1)
        end do
        jordm=ibid
!
!       Save the old REFD information in a temporary location
        call refdcp(nomres, '&&ORTH99')
!
!       Delete the old result concept
        call jedetc('G', nomres, 1)
    endif
    call rscrsd('G', nomres, 'MODE_MECA', nbmode)
!
!   If an existing concept was used, recuperate its reference information
    if (ier .ne. 0) call refdcp('&&ORTH99', nomres)
!
!-- CREATION DU REFD POUR SD_VERI, ET REUTILISATION ULTERIEURE
!    call jeexin(nomres(1:8)//'           .REFD', ibid)
!    if (ibid .eq. 0) then
!        concep(1) = intf
!        call refdaj('F', nomres, nbmode, numdd1, 'INTERF_DYNA', concep, ir)
!    endif
!
!
    iorne =0
    do i = 1, nbmode
        iorol = zi(jordm+i-1)
        iorne = iorne+1
!
        call rsexch(' ', nomres, 'DEPL', iorne, chamol,&
                    ier)
        call vtcrem(chamol, matras, 'G', 'R')
        call jeveuo(chamol//'.VALE', 'E', vr=vale)
        do ieq = 1, neq
            vale(ieq) = zr(idmode+(i-1)*neq+ieq-1)
        end do
        call rsnoch(nomres, 'DEPL', iorne)
!
        call rsadpa(base, 'L', 1, 'NUME_MODE', iorol,&
                    0, sjv=iad, styp=k8b)
        call rsadpa(nomres, 'E', 1, 'NUME_MODE', iorne,&
                    0, sjv=jiad, styp=k8b)
        zi(jiad) = zi(iad)
!
        call rsadpa(base, 'L', 1, 'FREQ', iorol,&
                    0, sjv=iad, styp=k8b)
        call rsadpa(nomres, 'E', 1, 'FREQ', iorne,&
                    0, sjv=jiad, styp=k8b)
        zr(jiad) = zr(iad)
!
        call rsadpa(base, 'L', 1, 'NORME', iorol,&
                    0, sjv=iad, styp=k8b)
        call rsadpa(nomres, 'E', 1, 'NORME', iorne,&
                    0, sjv=jiad, styp=k8b)
        zk24(jiad) = zk24(iad)
!
        call rsadpa(base, 'L', 1, 'OMEGA2', iorol,&
                    0, sjv=iad, styp=k8b)
        call rsadpa(nomres, 'E', 1, 'OMEGA2', iorne,&
                    0, sjv=jiad, styp=k8b)
        zr(jiad) = zr(iad)
!
        call rsadpa(base, 'L', 1, 'MASS_GENE', iorol,&
                    0, sjv=iad, styp=k8b)
        call rsadpa(nomres, 'E', 1, 'MASS_GENE', iorne,&
                    0, sjv=jiad, styp=k8b)
        zr(jiad) = zr(iad)
!
        call rsadpa(base, 'L', 1, 'RIGI_GENE', iorol,&
                    0, sjv=iad, styp=k8b)
        call rsadpa(nomres, 'E', 1, 'RIGI_GENE', iorne,&
                    0, sjv=jiad, styp=k8b)
        zr(jiad) = zr(iad)
!
        call rsadpa(base, 'L', 1, 'TYPE_MODE', iorol,&
                    0, sjv=iad, styp=k8b)
        call rsadpa(nomres, 'E', 1, 'TYPE_MODE', iorne,&
                    0, sjv=jiad, styp=k8b)
        zk16(jiad) = zk16(iad)
    end do
!
!
    AS_DEALLOCATE(vr=trav1)
    AS_DEALLOCATE(vr=trav3)
    AS_DEALLOCATE(vi=trav4)
    call jedetr('&&ORTH99.BASE')
    call jedetr('&&ORTH99.VECT_TEM')
!
!
999 continue
!
    call jedema()
end subroutine
