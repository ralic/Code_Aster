subroutine ajlipa(modelz, base)
    implicit none
#include "aster_types.h"
#include "jeveux.h"
#include "asterfort/asmpi_comm_vect.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/gcncon.h"
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
#include "asterfort/u2mesi.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
!
    character(len=*) :: modelz
    character(len=1) :: base
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
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
!  BUT :
!     CREATION (OU MODIFICATION) DE LA SD_PARTITION D'UN MODELE
!  REMARQUES :
!     * LA SD N'EST CREEE QUE DANS LE CAS DU PARALLELISME MPI DISTRIBUE
!     * IL FAUT APPELER CETTE ROUTINE APRES ADALIG SI CETTE DERNIERE
!       EST APPELEE (CAS DE OP0018)
! ----------------------------------------------------------------------
!
!
    character(len=8) :: modele, partit, ma, kbid, mopart, valk(3)
    character(len=19) :: ligrmo, sdfeti
    character(len=24) :: k24b, kdis
!
    integer :: i, rang, nbproc, ifm, niv, ibid, jpart, nbsd, nbma, jmail, ierd
    integer :: idd, nbmasd, i2, nmpp, nmp0, nmp0af, ico, nbpro1, krang, nmp1
    integer :: iexi
    integer :: icobis, jfdim, dist0, jnumsd, jparsd, jfeta, vali(3), nbmamo, ima
    integer :: nbgrel, jrepe, jfref, jprti, jprtk, nbsma
!
    logical :: plein0
!
    mpi_int :: mrank, msize
    data k24b /' '/
!
!-----------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
! ----------------------------------------------------------------------
!
!     VERIFICATIONS ET INITIALISATIONS
!
! ----------------------------------------------------------------------
!
    modele = modelz
    ligrmo = modele//'.MODELE'
!
!     -- S'IL N'Y A PAS D'ELEMENTS FINIS DANS LE MODELE :
!     ---------------------------------------------------
    call jeexin(ligrmo//'.LIEL', iexi)
    if (iexi .eq. 0) goto 99
!
    call jelira(ligrmo//'.LIEL', 'NUTIOC', nbgrel)
    call jeveuo(ligrmo//'.REPE', 'L', jrepe)
!
!     -- S'IL EXISTE DEJA UNE PARTITION, ON LA DETRUIT :
!     --------------------------------------------------
    call jeexin(modele//'.PARTIT', iexi)
    if (iexi .gt. 0) then
        call jeveuo(modele//'.PARTIT', 'E', jpart)
        partit = zk8(jpart-1+1)
        call detrsd('PARTITION', partit)
        zk8(jpart-1+1)=' '
    else
        call wkvect(modele//'.PARTIT', base//' V K8', 1, jpart)
    endif
!
!     -- S'IL N'A QU'UN SEUL PROC, IL N'Y A RIEN A FAIRE :
!     ----------------------------------------------------
    nbproc = 1
    rang = 0
    call asmpi_info(rank=mrank, size=msize)
    rang = to_aster_int(mrank)
    nbproc = to_aster_int(msize)
    if (nbproc .le. 1) goto 99
!
!     -- SI LE MODELE N'A PAS DE MAILLES, IL N'Y A RIEN A FAIRE :
!     -----------------------------------------------------------
    call jeexin(modele//'.MAILLE', iexi)
    if (iexi .eq. 0) goto 99
!
!     -- SI L'UTILISATEUR NE VEUT PAS DE DISTRIBUTION DES CALCULS,
!        IL N'Y A RIEN A FAIRE :
!     ------------------------------------------------------------
    call getvtx('PARTITION', 'PARALLELISME', iocc=1, scal=kdis, nbret=ibid)
    if (kdis .eq. 'CENTRALISE') goto 99
!
!     -- EN DISTRIBUE, LES SOUS-STRUCTURES SONT INTERDITES :
!     ------------------------------------------------------
    call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', ibid,&
                ma, ierd)
    call dismoi('F', 'NB_SM_MAILLA', ma, 'MAILLAGE', nbsma,&
                kbid, ierd)
    if (nbsma .gt. 0) then
        call u2mess('F', 'ALGORITH16_91')
    endif
!
! ----------------------------------------------------------------------
!
!     LECTURE DES MOT-CLES ET VERIFICATIONS SUPPLEMENTAIRES
!     CREATION DE LA SD
!
! ----------------------------------------------------------------------
!
    dist0 = 0
    sdfeti = ' '
    call gcncon('_', partit)
    zk8(jpart-1+1) = partit
!
!
!     CREATION DE LA PARTITION :
!     ----------------------------------------------------
    call jeveuo(modele//'.MAILLE', 'L', jmail)
    call jelira(modele//'.MAILLE', 'LONMAX', nbma)
    call wkvect(partit//'.PRTI', base//' V I', 1, jprti)
    zi(jprti-1+1)=nbproc
    call wkvect(partit//'.PRTK', base//' V K24', 2, jprtk)
    zk24(jprtk-1+1)= kdis
    if (kdis .ne. 'GROUP_ELEM') then
        call wkvect(partit//'.NUPROC.MAILLE', base//' V I', nbma+1, jnumsd)
        zi(jnumsd-1+nbma+1) = nbproc
!
!       NBMAMO : NBRE DE MAILLES DU MODELE
        nbmamo = 0
        do 10 ima = 1, nbma
            zi(jnumsd-1+ima) = -999
            if (zi(jmail-1+ima) .ne. 0) nbmamo = nbmamo+1
10      continue
    endif
!
!     -- RECUPERATIONS DES MOT-CLES :
!     -------------------------------
!
    if (kdis .eq. 'SOUS_DOMAINE') then
        call getvis('PARTITION', 'CHARGE_PROC0_SD', iocc=1, scal=dist0, nbret=ibid)
        call getvid('PARTITION', 'PARTITION', iocc=1, scal=sdfeti, nbret=ibid)
        if (ibid .eq. 1) zk24(jprtk-1+2)= sdfeti
    else if (kdis(1:4).eq.'MAIL') then
        call getvis('PARTITION', 'CHARGE_PROC0_MA', iocc=1, scal=dist0, nbret=ibid)
    endif
!
!     -- VERIFICATION POUR LE CAS DU PARTITIONNEMENT EN SOUS-DOMAINES :
!     -----------------------------------------------------------------
    if (kdis .eq. 'SOUS_DOMAINE') then
        call jeveuo(sdfeti//'.FREF', 'L', jfref)
        mopart = zk8(jfref-1+1)
        if (modele .ne. mopart) then
            valk(1) = sdfeti(1:8)
            valk(2) = modele
            valk(3) = mopart
            call u2mesk('F', 'ALGORITH17_17', 3, valk)
        endif
    endif
!
!     -- VERIFICATIONS SUR LE NOMBRE DE MAILLES OU DE SOUS-DOMAINES :
!        PAR RAPPORT AU NOMBRE DE PROCESSEURS
!     ---------------------------------------------------------------
    if (kdis .eq. 'SOUS_DOMAINE') then
        call jeveuo(sdfeti//'.FDIM', 'L', jfdim)
        nbsd = zi(jfdim-1+1)
!       IL FAUT AU MOINS UN SD PAR PROC HORS PROC0
        if (((nbsd-dist0).lt.(nbproc-1)) .and. (dist0.gt.0)) then
            call u2mess('F', 'ALGORITH16_99')
        endif
        if ((nbsd.lt.nbproc) .and. (dist0.eq.0)) then
            vali(1) = nbsd
            vali(2) = nbproc
            call u2mesi('F', 'ALGORITH17_1', 2, vali)
        endif
    else if (kdis(1:4).eq.'MAIL') then
!       IL FAUT AU MOINS UNE MAILLE PAR PROC
        if (nbmamo .lt. nbproc) then
            vali(1) = nbmamo
            vali(2) = nbproc
            call u2mesi('F', 'ALGORITH16_93', 2, vali)
        endif
    else if (kdis.eq.'GROUP_ELEM') then
!       IL FAUT AU MOINS UN GREL PAR PROC
        if (nbgrel .lt. nbproc) then
            vali(1) = nbgrel
            vali(2) = nbproc
            call u2mesi('F', 'ALGORITH16_97', 2, vali)
        endif
    else
        ASSERT(.false.)
    endif
!
! ----------------------------------------------------------------------
!
!     REMPLISSAGE DE LA SD
!
! ----------------------------------------------------------------------
!
!
    if (kdis .eq. 'SOUS_DOMAINE') then
!     --------------------------------
        call wkvect('&&AJLIPA.PARTITION.SD', 'V V I', nbsd, jparsd)
        call sdpart(nbsd, dist0, zi(jparsd))
        do 30 idd = 1, nbsd
            if (zi(jparsd-1+idd) .eq. 1) then
                call jeveuo(jexnum(sdfeti//'.FETA', idd), 'L', jfeta)
                call jelira(jexnum(sdfeti//'.FETA', idd), 'LONMAX', nbmasd)
                do 20 i = 1, nbmasd
                    i2 = zi(jfeta-1+i)
                    if (zi(jnumsd-1+i2) .ne. -999) then
!               -- MAILLE COMMUNE A PLUSIEURS SOUS-DOMAINES
                        vali(1) = i2
                        call u2mesi('F', 'ALGORITH16_98', 1, vali)
                    else
                        zi(jnumsd-1+i2) = rang
                    endif
20              continue
            endif
30      continue
        call asmpi_comm_vect('MPI_MAX', 'I', nbval=nbma, vi=zi(jnumsd))
        call jedetr('&&AJLIPA.PARTITION.SD')
!
!
    else if (kdis.eq.'MAIL_DISPERSE') then
!     -------------------------------------
!       -- LE PROC 0 A UNE CHARGE DIFFERENTE DES AUTRES (DIST0) :
!       NMPP NBRE DE MAILLES PAR PROC (A LA LOUCHE)
        nmpp = max(1,nbmamo/nbproc)
!       NMP0 NBRE DE MAILLES AFFECTEES AU PROC0 (A LA LOUCHE)
        nmp0 = (dist0*nmpp)/100
!
!       -- AFFECTATION DES MAILLES AUX DIFFERENTS PROCS :
        nmp0af = 0
        ico = 0
        nbpro1 = nbproc
        plein0 = .false.
        do 40,ima = 1,nbma
        if (zi(jmail-1+ima) .eq. 0) goto 40
        ico = ico+1
        krang = mod(ico,nbpro1)
        if (plein0) krang = krang+1
        if (krang .eq. 0) nmp0af = nmp0af+1
        zi(jnumsd-1+ima) = krang
        if (nmp0af .eq. nmp0) then
            plein0 = .true.
            nbpro1 = nbproc-1
        endif
40      continue
!
!
    else if (kdis.eq.'MAIL_CONTIGU') then
!       ----------------------------------
!       NMP0 NBRE DE MAILLES AFFECTEES AU PROC0 :
        nmpp = max(1,nbmamo/nbproc)
        nmp0 = (dist0*nmpp)/100
        nmp1 = ((nbmamo-nmp0)/(nbproc-1))+1
!
!       -- AFFECTATION DES MAILLES AUX DIFFERENTS PROCS :
!          ON AFFECTE LES 1ERES MAILLES AU PROC0 PUIS LES AUTRES
!          AUX AUTRES PROCS.
        nmpp = nmp0
        krang = 0
        ico = 0
        do 50,ima = 1,nbma
        if (zi(jmail-1+ima) .eq. 0) goto 50
        ico = ico+1
!         -- ON CHANGE DE PROC :
        if (ico .gt. nmpp) then
            ico = 1
            nmpp = nmp1
            krang = krang+1
        endif
        zi(jnumsd-1+ima) = krang
50      continue
!
!       -- ON VERIFIE QUE TOUTES LES MAILLES SONT DISTRIBUEES :
        ico = 0
        icobis = 0
        do 60 i = 1, nbma
            if (zi(jnumsd-1+i) .ge. 0) ico = ico+1
            if (zi(jnumsd-1+i) .eq. rang) icobis = icobis+1
60      continue
        ASSERT(ico.eq.nbmamo)
!
!
    else if (kdis.eq.'GROUP_ELEM') then
!     ----------------------------------
!       -- IL N'Y A RIEN A FAIRE !
!       SI KDIS='GROUP_ELEM', LA REGLE POUR LES CALCULS ELEMENTAIRES
!       ET LES ASSEMBLAGES EST :
!       QUELQUE SOIT LE LIGREL (MODELE, CHARGE, ....) :
!       LE GREL IGREL EST TRAITE PAR LE PROCESSEUR
!       DE RANG=MOD(IGREL,NBPROC)
!
    else
        ASSERT(.false.)
    endif
!
!
99  continue
!
    call jedema()
end subroutine
