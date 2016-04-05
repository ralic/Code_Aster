subroutine ajlipa(modelz, base, kdis, sd_partit1z)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/asmpi_comm_vect.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/gcncon.h"
#include "asterc/getres.h"
#include "asterfort/fetcrf.h"
#include "asterfort/fetskp.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/sdpart.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"

    character(len=*), intent(in) :: modelz
    character(len=1), intent(in) :: base
    character(len=24), intent(in) :: kdis
    character(len=*), intent(in) :: sd_partit1z
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
!  But :
!     Creation (ou modification) de la sd_partition d'un modele
!     (Commandes AFFE_MODELE et MODI_MODELE)
!  Remarques :
!     * la sd n'est creee que dans le cas du parallelisme mpi distribue
!     * il faut appeler cette routine apres adalig si cette derniere
!       est appelee (cas de op0018)
! ----------------------------------------------------------------------
    character(len=8) :: modele, partit, mopart, valk(3), nomres, methode
    character(len=16) :: typres, nomcom
    character(len=19) :: ligrmo, sd_partit1
    character(len=24) :: k24b
    integer :: i, rang, nbproc, ifm, niv, ibid, jpart, nbsd, nbma
    integer :: idd, nbmasd, i2, nmpp, nmp0, nmp0af, ico, nbpro1, krang, nmp1
    integer :: iexi, nbpart
    integer :: icobis, dist0, jnumsd, jparsd, jfeta, vali(3), nbmamo, ima
    integer :: nbgrel, jrepe, jprti, jprtk
    aster_logical :: plein0, exi_sdpart1
    integer, pointer :: fdim(:) => null()
    character(len=8), pointer :: fref(:) => null()
    integer, pointer :: maille(:) => null()
    mpi_int :: mrank, msize
    data k24b /' '/

!-----------------------------------------------------------------------
    call jemarq()
    call infniv(ifm, niv)

! ----------------------------------------------------------------------
!   --  Verifications et initialisations
! ----------------------------------------------------------------------
    modele = modelz
    ligrmo = modele//'.MODELE'
    sd_partit1=sd_partit1z

    call getres(nomres, typres, nomcom)
    ASSERT(nomres.eq.modele)
    ASSERT(nomcom.eq.'AFFE_MODELE' .or. nomcom.eq.'MODI_MODELE')

!   -- s'il existe deja une partition, on la detruit :
!   --------------------------------------------------
    call jeexin(modele//'.PARTIT', iexi)
    if (iexi .gt. 0) then
        ASSERT(nomcom.eq.'MODI_MODELE')
        call jeveuo(modele//'.PARTIT', 'E', jpart)
        partit = zk8(jpart-1+1)
        call detrsd('PARTITION', partit)
        zk8(jpart-1+1)=' '
    else
        call wkvect(modele//'.PARTIT', base//' V K8', 1, jpart)
    endif

!   -- s'il n'y a pas d'elements finis dans le modele :
!   ---------------------------------------------------
    call jeexin(ligrmo//'.LIEL', iexi)
    if (iexi .eq. 0) goto 999

!   -- s'il n'a qu'un seul proc, il n'y a rien a faire :
!   ----------------------------------------------------
    nbproc = 1
    rang = 0
    call asmpi_info(rank=mrank, size=msize)
    rang = to_aster_int(mrank)
    nbproc = to_aster_int(msize)
    if (nbproc .le. 1) goto 999

!   -- si le modele n'a pas de mailles, il n'y a rien a faire :
!   -----------------------------------------------------------
    call jeexin(modele//'.MAILLE', iexi)
    if (iexi .eq. 0) goto 999

!   -- si l'utilisateur ne veut pas de distribution des calculs,
!      il n'y a rien a faire :
!   ------------------------------------------------------------
    if (kdis .eq. 'CENTRALISE') goto 999


!   -- la sd_partit1 est a fournir si 'SOUS_DOMAINE' ou 'GROUP_ELEM+'
!   -------------------------------------------------------------------
    exi_sdpart1=(kdis .eq. 'GROUP_ELEM+' .or. kdis .eq. 'SOUS_DOMAINE')
    if (exi_sdpart1) then
        ASSERT(sd_partit1.ne.' ')
    else
        ASSERT(sd_partit1.eq.' ')
    endif


! ----------------------------------------------------------------------
!   Lecture des mot-cles et verifications supplementaires
!   Creation de la sd_partit1
! ----------------------------------------------------------------------
    dist0 = 0

    call gcncon('_', partit)
    zk8(jpart-1+1) = partit


!   -- Creation de la sd_partition :
!   ----------------------------------------------------
    call jeveuo(modele//'.MAILLE', 'L', vi=maille)
    call jelira(modele//'.MAILLE', 'LONMAX', nbma)
    call wkvect(partit//'.PRTI', base//' V I', 1, jprti)
    zi(jprti-1+1)=nbproc
    call wkvect(partit//'.PRTK', base//' V K24', 2, jprtk)
    zk24(jprtk-1+1)= kdis
    if (kdis(1:5) .eq. 'MAIL_'.or. kdis .eq. 'SOUS_DOMAINE') then
        call wkvect(partit//'.NUPROC.MAILLE', base//' V I', nbma+1, jnumsd)
        zi(jnumsd-1+nbma+1) = nbproc

!       nbmamo : nbre de mailles du modele
        nbmamo = 0
        do ima = 1, nbma
            zi(jnumsd-1+ima) = -999
            if (maille(ima) .ne. 0) nbmamo = nbmamo+1
        end do
    endif


!   -- Recuperations des mot-cles :
!   -------------------------------
    if (exi_sdpart1) then
        call getvis('PARTITION', 'CHARGE_PROC0_SD', iocc=1, scal=dist0, nbret=ibid)
        if (ibid.eq.0) dist0=0
        ASSERT(sd_partit1.ne.' ')
        zk24(jprtk-1+2)= sd_partit1

    else if (kdis(1:5).eq.'MAIL_') then
        call getvis('PARTITION', 'CHARGE_PROC0_MA', iocc=1, scal=dist0, nbret=ibid)
        ASSERT(sd_partit1.eq.' ')
    endif


!   -- Verification pour le cas du partitionnement avec sd_partit1 :
!   -----------------------------------------------------------------
    if (exi_sdpart1) then
        call jeveuo(sd_partit1//'.FREF', 'L', vk8=fref)
        mopart = fref(1)
        if (modele .ne. mopart) then
            valk(1) = sd_partit1(1:8)
            valk(2) = modele
            valk(3) = mopart
            call utmess('F', 'PARTITION1_17', nk=3, valk=valk)
        endif
    endif


!   -- Verifications sur le nombre de mailles ou de sous-domaines :
!      par rapport au nombre de processeurs
!   ---------------------------------------------------------------
    if (exi_sdpart1) then
        call jeveuo(sd_partit1//'.FDIM', 'L', vi=fdim)
        nbsd = fdim(1)
!       il faut au moins un sd par proc hors proc0
        if (((nbsd-dist0).lt.(nbproc-1)) .and. (dist0.gt.0)) then
            call utmess('F', 'PARTITION1_99')
        endif
        if ((nbsd.lt.nbproc) .and. (dist0.eq.0)) then
            vali(1) = nbsd
            vali(2) = nbproc
            call utmess('F', 'PARTITION1_1', ni=2, vali=vali)
        endif
    else if (kdis(1:5).eq.'MAIL_') then
!       il faut au moins une maille par proc
        if (nbmamo .lt. nbproc) then
            vali(1) = nbmamo
            vali(2) = nbproc
            call utmess('F', 'PARTITION1_93', ni=2, vali=vali)
        endif
    endif


! ----------------------------------------------------------------------
!   Remplissage de la sd
! ----------------------------------------------------------------------

    if (kdis .eq. 'SOUS_DOMAINE') then
!   ----------------------------------
        call wkvect('&&AJLIPA.PARTITION.SD', 'V V I', nbsd, jparsd)
        call sdpart(nbsd, dist0, zi(jparsd))
        do idd = 1, nbsd
            if (zi(jparsd-1+idd) .eq. 1) then
                call jeveuo(jexnum(sd_partit1//'.FETA', idd), 'L', jfeta)
                call jelira(jexnum(sd_partit1//'.FETA', idd), 'LONMAX', nbmasd)
                do i = 1, nbmasd
                    i2 = zi(jfeta-1+i)
                    if (zi(jnumsd-1+i2) .ne. -999) then
!                       -- maille commune a plusieurs sous-domaines
                        vali(1) = i2
                        call utmess('F', 'PARTITION1_98', si=vali(1))
                    else
                        zi(jnumsd-1+i2) = rang
                    endif
                end do
            endif
        end do
        call asmpi_comm_vect('MPI_MAX', 'I', nbval=nbma, vi=zi(jnumsd))
        call jedetr('&&AJLIPA.PARTITION.SD')


    else if (kdis.eq.'MAIL_DISPERSE') then
!   ---------------------------------------
!       -- le proc 0 a une charge differente des autres (dist0) :
!       nmpp nbre de mailles par proc (a la louche)
        nmpp = max(1,nbmamo/nbproc)
!       nmp0 nbre de mailles affectees au proc0 (a la louche)
        nmp0 = (dist0*nmpp)/100

!       -- affectation des mailles aux differents procs :
        nmp0af = 0
        ico = 0
        nbpro1 = nbproc
        plein0 = .false.
        do ima = 1, nbma
            if (maille(ima) .eq. 0) goto 40
            ico = ico+1
            krang = mod(ico,nbpro1)
            if (plein0) krang = krang+1
            if (krang .eq. 0) nmp0af = nmp0af+1
            zi(jnumsd-1+ima) = krang
            if (nmp0af .eq. nmp0) then
                plein0 = .true.
                nbpro1 = nbproc-1
            endif
 40         continue
        end do


    else if (kdis.eq.'MAIL_CONTIGU') then
!   --------------------------------------
!       nmp0 nbre de mailles affectees au proc0 :
        nmpp = max(1,nbmamo/nbproc)
        nmp0 = (dist0*nmpp)/100
        nmp1 = ((nbmamo-nmp0)/(nbproc-1))+1

!       -- affectation des mailles aux differents procs :
!          on affecte les 1eres mailles au proc0 puis les autres
!          aux autres procs.
        nmpp = nmp0
        krang = 0
        ico = 0
        do ima = 1, nbma
            if (maille(ima) .eq. 0) goto 50
            ico = ico+1
!         -- on change de proc :
            if (ico .gt. nmpp) then
                ico = 1
                nmpp = nmp1
                krang = krang+1
            endif
            zi(jnumsd-1+ima) = krang
 50         continue
        end do

!       -- on verifie que toutes les mailles sont distribuees :
        ico = 0
        icobis = 0
        do i = 1, nbma
            if (zi(jnumsd-1+i) .ge. 0) ico = ico+1
            if (zi(jnumsd-1+i) .eq. rang) icobis = icobis+1
        end do
        ASSERT(ico.eq.nbmamo)


    else if (kdis.eq.'GROUP_ELEM' .or. kdis.eq.'GROUP_ELEM+') then
!   ----------------------------------------------------------------
!       -- il n'y a rien a faire !
!       La regle pour les calculs elementaires et les assemblages est :
!       quelque soit le ligrel (modele, charge, ....) :
!       le grel igrel est traite par le processeur
!       de rang=mod(igrel,nbproc)

    else
        ASSERT(.false.)
    endif


999 continue

    call jedema()
end subroutine
