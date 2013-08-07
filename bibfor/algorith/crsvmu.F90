subroutine crsvmu(motfac, solveu, istop, nprec, syme,&
                  epsmat, mixpre, kmd)
    implicit none
#include "aster_types.h"
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterc/getvid.h"
#include "asterc/getvis.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/asmpi_comm_jev.h"
#include "asterfort/u2mesi.h"
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
    integer :: ibid, ifm, niv, i, pcpiv, nbproc, rang, iaux, jrefa
    integer :: monit(12), jnumsd, n1, vali(2), compt
    integer :: jmail, nbma, islvk, islvr, islvi, iret
    real(kind=8) :: eps
    character(len=4) :: klag2
    character(len=8) :: ktypr, ktyps, ktyprn, ktypp, modele, partit, matra
    character(len=12) :: kooc
    character(len=19) :: k19b
    character(len=24) :: kmonit(12)
    integer :: eximo1, eximo2, eximo3, eximc, eximod, jprti, jprtk
    integer :: iarg, jpart, iexi
    logical :: ldgrel
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
            call getvid(' ', 'MODELE', 1, iarg, 1,&
                        modele, ibid)
            if (ibid .ne. 1) goto 70
        else
            call getvid(' ', 'MATR_RIGI', 1, iarg, 1,&
                        matra, ibid)
            call getvid(' ', 'MATR_A', 1, iarg, 1,&
                        matra, ibid)
            if (ibid .ne. 1) goto 70
            k19b=' '
            k19b=matra
            call jeveuo(k19b//'.REFA', 'L', jrefa)
            if (zk24(jrefa+9)(1:4) .eq. 'GENE') then
!         CAS PARTICULIER DU NUME_DDL_GENE
                goto 70
            else if (zk24(jrefa+9)(1:4).eq.'NOEU') then
                call dismoi('F', 'NOM_MODELE', matra, 'MATR_ASSE', ibid,&
                            modele, iret)
            else
! --- CAS NON PREVU
                ASSERT(.false.)
            endif
        endif
!
!       -- PARTITION POUR LE PARALLELISME :
        partit=' '
        call jeexin(modele//'.PARTIT', iexi)
        if (iexi .gt. 0) then
            call jeveuo(modele//'.PARTIT', 'L', jpart)
            if (zk8(jpart-1+1) .ne. ' ') then
                partit=zk8(jpart-1+1)
            endif
        endif
!
        if (partit .ne. ' ') then
!         -- CALCUL DISTRIBUE :
            call jeveuo(partit//'.PRTI', 'L', jprti)
            if (zi(jprti) .ne. nbproc) then
                vali(1)=zi(jprti)
                vali(2)=nbproc
                call u2mesi('F', 'CALCULEL_13', 2, vali)
            endif
!
            call jeveuo(partit//'.PRTK', 'L', jprtk)
            ldgrel=zk24(jprtk-1+1).eq.'GROUP_ELEM'
            if (.not.ldgrel) then
                call jeveuo(partit//'.NUPROC.MAILLE', 'L', jnumsd)
                call jelira(partit//'.NUPROC.MAILLE', 'LONMAX', n1)
                ASSERT(zi(jnumsd-1+n1).eq.nbproc)
                nbma=n1-1
                compt=0
                do 50 i = 1, nbma
                    if (zi(jnumsd-1+i) .eq. rang) compt=compt+1
50              continue
            endif
        else
!       -- CENTRALISE
            call jeveuo(modele//'.MAILLE', 'L', jmail)
            call jelira(modele//'.MAILLE', 'LONMAX', nbma)
            compt=0
            do 60 i = 1, nbma
                if (zi(jmail-1+i) .ne. 0) compt=compt+1
60          continue
        endif
    endif
!
!
! --- OBJETS DE MONITORING
! --- INDIRECTION SI ON N'A PAS PU LIRE LE MODELE (NUME_DDL_GENE)
70  continue
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
        do 110 i = 1, nbproc
            zi(monit(1)+i-1)=0
            zi(monit(2)+i-1)=0
            zi(monit(9)+i-1)=0
            zi(monit(10)+i-1)=0
            zi(monit(11)+i-1)=0
            zi(monit(12)+i-1)=0
110      continue
! -----
        zi(monit(9)+rang)=compt
        call asmpi_comm_jev('REDUCE', kmonit(9))
! ----- CORRECTION SI MODAL
        if (eximo2 .eq. 1) then
            iaux=0
            do 112 i = 1, nbproc
                iaux=iaux+zi(monit(9)+i-1)
112          continue
            do 114 i = 1, nbproc
                zi(monit(9)+i-1)=iaux
114          continue
        endif
    endif
!
! --- LECTURES PARAMETRES DEDIES AU SOLVEUR
    call getvis(motfac, 'PCENT_PIVOT', 1, iarg, 1,&
                pcpiv, ibid)
    ASSERT(ibid.eq.1)
    call getvtx(motfac, 'TYPE_RESOL', 1, iarg, 1,&
                ktypr, ibid)
    ASSERT(ibid.eq.1)
    call getvtx(motfac, 'PRETRAITEMENTS', 1, iarg, 1,&
                ktyps, ibid)
    ASSERT(ibid.eq.1)
!
    ktypp='SANS'
    eximc=getexm(motfac,'POSTTRAITEMENTS')
    if (eximc .eq. 1) then
        call getvtx(motfac, 'POSTTRAITEMENTS', 1, iarg, 1,&
                    ktypp, ibid)
    endif
!
    call getvtx(motfac, 'RENUM', 1, iarg, 1,&
                ktyprn, ibid)
    ASSERT(ibid.eq.1)
    call getvtx(motfac, 'ELIM_LAGR2', 1, iarg, 1,&
                klag2, ibid)
    ASSERT(ibid.eq.1)
!
    eps=-1.d0
    eximc=getexm(motfac,'RESI_RELA')
    if (eximc .eq. 1) then
        call getvr8(motfac, 'RESI_RELA', 1, iarg, 1,&
                    eps, ibid)
    endif
!
    call getvtx(motfac, 'GESTION_MEMOIRE', 1, iarg, 1,&
                kooc, ibid)
    ASSERT(ibid.eq.1)
!
! --- ON REMPLIT LA SD_SOLVEUR
! --- ATTENTION A LA COHERENCE AVEC CRSMSP
!
    call jeveuo(solveu//'.SLVK', 'E', islvk)
    call jeveuo(solveu//'.SLVR', 'E', islvr)
    call jeveuo(solveu//'.SLVI', 'E', islvi)
!
    zk24(islvk-1+1) = 'MUMPS'
    zk24(islvk-1+2) = ktyps
    zk24(islvk-1+3) = ktypr
    zk24(islvk-1+4) = ktyprn
    zk24(islvk-1+5) = syme
    zk24(islvk-1+6) = klag2
    zk24(islvk-1+7) = mixpre
    zk24(islvk-1+8) = 'NON'
    zk24(islvk-1+9) = kooc
    zk24(islvk-1+10) = kmd
    zk24(islvk-1+11) = ktypp
    zk24(islvk-1+12) = 'XXXX'
!
    zr(islvr-1+1) = epsmat
    zr(islvr-1+2) = eps
    zr(islvr-1+3) = 0.d0
    zr(islvr-1+4) = 0.d0
!
    zi(islvi-1+1) = nprec
    zi(islvi-1+2) = pcpiv
    zi(islvi-1+3) = istop
    zi(islvi-1+4) = -9999
    zi(islvi-1+5) = -9999
    zi(islvi-1+6) = -9999
    zi(islvi-1+7) = -9999
    zi(islvi-1+8) = 0
!
    call jedema()
end subroutine
