subroutine crsvmu(motfac, solveu, istop, nprec, syme,&
                  epsmat, mixpre, kmd)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterfort/asmpi_comm_jev.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: istop, nprec
    real(kind=8) :: epsmat
    character(len=3) :: syme, mixpre, kmd
    character(len=16) :: motfac
    character(len=19) :: solveu
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
! ----------------------------------------------------------
!  BUT : REMPLISSAGE SD_SOLVEUR MUMPS
!        ATTENTION A LA COHERENCE AVEC CRSMSP ET CRSINT
!
! IN K19 SOLVEU  : NOM DU SOLVEUR DONNE EN ENTREE
! OUT    SOLVEU  : LE SOLVEUR EST CREE ET INSTANCIE
! IN  IN ISTOP   : PARAMETRE LIE AUX MOT-CLE STOP_SINGULIER
! IN  IN NPREC   :                           NPREC
! IN  K3 SYME    :                           SYME
! IN  R8 EPSMAT  :                           FILTRAGE_MATRICE
! IN  K3 MIXPRE  :                           MIXER_PRECISION
! IN  K3 KMD     :                           MATR_DISTRIBUEE
! ----------------------------------------------------------
!
    integer :: ibid, ifm, niv, i, pcpiv, nbproc, rang, iaux
    integer :: monit(12),  n1, vali(2), compt
    integer ::  nbma
    real(kind=8) :: eps
    character(len=5) :: klag2
    character(len=8) :: ktypr, ktyps, ktyprn, ktypp, modele, partit, matra
    character(len=12) :: kooc
    character(len=19) :: k19b
    character(len=24) :: kmonit(12)
    integer :: eximo1, eximo2, eximo3, eximc, eximod
    integer ::  iexi
    logical(kind=1) :: ldgrel
    character(len=8), pointer :: vpartit(:) => null()
    real(kind=8), pointer :: slvr(:) => null()
    character(len=24), pointer :: prtk(:) => null()
    character(len=24), pointer :: slvk(:) => null()
    integer, pointer :: prti(:) => null()
    character(len=24), pointer :: refa(:) => null()
    integer, pointer :: mail(:) => null()
    integer, pointer :: numsd(:) => null()
    integer, pointer :: slvi(:) => null()
    mpi_int :: mrank, msize
!------------------------------------------------------------------
    call jemarq()
!
! --- INIT
    call infniv(ifm, niv)
    rang=0
    nbproc=1
    if (niv .ge. 2) then
        call asmpi_info(rank=mrank, size=msize)
        rang = to_aster_int(mrank)
        nbproc = to_aster_int(msize)
    endif
!
! --- POUR MONITORING: RECHERCHE DU NBRE DE MAILLES PAR PROC
!     SI INF>1 ET SI EXISTENCE D'UN MODELE
! --- 1ER CAS DE FIGURE: OPERATEUR A MOT-CLE MODELE (QUASI-STATIQUE)
!     2ND CAS DE FIGURE:                     MATR_RIGI OU MATR_A (MODAL)
    eximod=0
    eximo1=0
    eximo1=getexm(' ','MODELE')
    eximo2=0
    eximo2=getexm(' ','MATR_RIGI')
    eximo3=0
    eximo3=getexm(' ','MATR_A')
    if ((eximo1.eq.1) .or. (eximo2.eq.1) .or. (eximo3.eq.1)) eximod=1
    compt=-9999
    if ((eximod.eq.1) .and. (niv.ge.2)) then
        if (eximo1 .eq. 1) then
            call getvid(' ', 'MODELE', scal=modele, nbret=ibid)
            if (ibid .ne. 1) goto 70
        else
            matra=' '
            if (eximo2.eq.1) call getvid(' ', 'MATR_RIGI', scal=matra, nbret=ibid)
            if (eximo3.eq.1) call getvid(' ', 'MATR_A', scal=matra, nbret=ibid)
            if (matra .eq. ' ') goto 70
            k19b=matra
            call jeveuo(k19b//'.REFA', 'L', vk24=refa)
            if (refa(10)(1:4) .eq. 'GENE') then
!               --  CAS PARTICULIER DU NUME_DDL_GENE
                goto 70
            else if (refa(10)(1:4).eq.'NOEU') then
                call dismoi('NOM_MODELE', matra, 'MATR_ASSE', repk=modele)
            else
!               --- CAS NON PREVU
                ASSERT(.false.)
            endif
        endif
!
!       -- PARTITION POUR LE PARALLELISME :
        partit=' '
        call jeexin(modele//'.PARTIT', iexi)
        if (iexi .gt. 0) then
            call jeveuo(modele//'.PARTIT', 'L', vk8=vpartit)
            if (vpartit(1) .ne. ' ') then
                partit=vpartit(1)
            endif
        endif
!
        if (partit .ne. ' ') then
!         -- CALCUL DISTRIBUE :
            call jeveuo(partit//'.PRTI', 'L', vi=prti)
            if (prti(1) .ne. nbproc) then
                vali(1)=prti(1)
                vali(2)=nbproc
                call utmess('F', 'CALCULEL_13', ni=2, vali=vali)
            endif
!
            call jeveuo(partit//'.PRTK', 'L', vk24=prtk)
            ldgrel=prtk(1).eq.'GROUP_ELEM'
            if (.not.ldgrel) then
                call jeveuo(partit//'.NUPROC.MAILLE', 'L', vi=numsd)
                call jelira(partit//'.NUPROC.MAILLE', 'LONMAX', n1)
                ASSERT(numsd(n1).eq.nbproc)
                nbma=n1-1
                compt=0
                do i = 1, nbma
                    if (numsd(i) .eq. rang) compt=compt+1
                end do
            endif
        else
!       -- CENTRALISE
            call jeveuo(modele//'.MAILLE', 'L', vi=mail)
            call jelira(modele//'.MAILLE', 'LONMAX', nbma)
            compt=0
            do i = 1, nbma
                if (mail(i) .ne. 0) compt=compt+1
            end do
        endif
    endif
!
!
! --- OBJETS DE MONITORING
! --- INDIRECTION SI ON N'A PAS PU LIRE LE MODELE (NUME_DDL_GENE)
 70 continue
    if (niv .ge. 2) then
        kmonit(1)='&MUMPS.INFO.MAILLE'
        kmonit(2)='&MUMPS.INFO.MEMOIRE'
        kmonit(9)='&MUMPS.NB.MAILLE'
        kmonit(10)='&MUMPS.INFO.MEM.EIC'
        kmonit(11)='&MUMPS.INFO.MEM.EOC'
        kmonit(12)='&MUMPS.INFO.MEM.USE'
        call wkvect(kmonit(1), 'V V I', nbproc, monit(1))
        call wkvect(kmonit(2), 'V V I', nbproc, monit(2))
        call wkvect(kmonit(9), 'V V I', nbproc, monit(9))
        call wkvect(kmonit(10), 'V V I', nbproc, monit(10))
        call wkvect(kmonit(11), 'V V I', nbproc, monit(11))
        call wkvect(kmonit(12), 'V V I', nbproc, monit(12))
        do i = 1, nbproc
            zi(monit(1)+i-1)=0
            zi(monit(2)+i-1)=0
            zi(monit(9)+i-1)=0
            zi(monit(10)+i-1)=0
            zi(monit(11)+i-1)=0
            zi(monit(12)+i-1)=0
        end do
! -----
        zi(monit(9)+rang)=compt
        call asmpi_comm_jev('REDUCE', kmonit(9))
! ----- CORRECTION SI MODAL
        if (eximo2 .eq. 1) then
            iaux=0
            do i = 1, nbproc
                iaux=iaux+zi(monit(9)+i-1)
            end do
            do i = 1, nbproc
                zi(monit(9)+i-1)=iaux
            end do
        endif
    endif
!
! --- LECTURES PARAMETRES DEDIES AU SOLVEUR
    call getvis(motfac, 'PCENT_PIVOT', iocc=1, scal=pcpiv, nbret=ibid)
    ASSERT(ibid.eq.1)
    call getvtx(motfac, 'TYPE_RESOL', iocc=1, scal=ktypr, nbret=ibid)
    ASSERT(ibid.eq.1)
    call getvtx(motfac, 'PRETRAITEMENTS', iocc=1, scal=ktyps, nbret=ibid)
    ASSERT(ibid.eq.1)
!
    ktypp='SANS'
    eximc=getexm(motfac,'POSTTRAITEMENTS')
    if (eximc .eq. 1) then
        call getvtx(motfac, 'POSTTRAITEMENTS', iocc=1, scal=ktypp, nbret=ibid)
    endif
!
    call getvtx(motfac, 'RENUM', iocc=1, scal=ktyprn, nbret=ibid)
    ASSERT(ibid.eq.1)
    call getvtx(motfac, 'ELIM_LAGR', iocc=1, scal=klag2, nbret=ibid)
    ASSERT(ibid.eq.1)
!
    eps=-1.d0
    eximc=getexm(motfac,'RESI_RELA')
    if (eximc .eq. 1) then
        call getvr8(motfac, 'RESI_RELA', iocc=1, scal=eps, nbret=ibid)
    endif
!
    call getvtx(motfac, 'GESTION_MEMOIRE', iocc=1, scal=kooc, nbret=ibid)
    ASSERT(ibid.eq.1)
!
! --- ON REMPLIT LA SD_SOLVEUR
! --- ATTENTION A LA COHERENCE AVEC CRSMSP
!
    call jeveuo(solveu//'.SLVK', 'E', vk24=slvk)
    call jeveuo(solveu//'.SLVR', 'E', vr=slvr)
    call jeveuo(solveu//'.SLVI', 'E', vi=slvi)
!
    slvk(1) = 'MUMPS'
    slvk(2) = ktyps
    slvk(3) = ktypr
    slvk(4) = ktyprn
    slvk(5) = syme
    slvk(6) = klag2
    slvk(7) = mixpre
    slvk(8) = 'NON'
    slvk(9) = kooc
    slvk(10) = kmd
    slvk(11) = ktypp
    slvk(12) = 'XXXX'
!
    slvr(1) = epsmat
    slvr(2) = eps
    slvr(3) = 0.d0
    slvr(4) = 0.d0
!
    slvi(1) = nprec
    slvi(2) = pcpiv
    slvi(3) = istop
    slvi(4) = -9999
    slvi(5) = -9999
    slvi(6) = 1
    slvi(7) = -9999
    slvi(8) = 0
!
    call jedema()
end subroutine
