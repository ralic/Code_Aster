subroutine ldlt_renum(nu1z,nu2z,perm,basp)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/infniv.h"
#include "asterfort/infbav.h"
#include "asterfort/infmue.h"
#include "asterfort/jemarq.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/copisd.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/nueffe.h"
#include "asterfort/dismoi.h"
#include "asterfort/promor.h"
#include "asterfort/detrsd.h"
#include "asterc/getres.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
!
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

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
    character(len=*), intent(in) :: nu1z
    character(len=*), intent(in), optional :: nu2z
    character(len=24), intent(in), optional :: perm
    character(len=1),  intent(in), optional :: basp

! --------------------------------------------------------------------------------------------------
! Fonctions :
! ------------
!  Cette routine sert a renumeroter une matrice pour que la factorisation LDLT
!  soit plus efficace. La méthode de renumerotation que l'on essaye d'utiliser est 'RCMK'
!
!  1) Si nu2z est absent :
!     Ajouter les objets .M2LC et .LC2M dans la sd_nume_ddl nu1z
!
!     Ces 2 objets etablissent la correspondance entre les numerotations du stockage Morse (.VALM)
!     et du stockage Ligne de Ciel (.UALF). Ces deux vecteurs sont reciproques l'un de l'autre :
!       * ieqlc=.M2LC(ieqm)
!       * ieqm =.LC2M(ieqlc)
!     Ces 2 objets sont crees sur la meme base (G/V) que la base de nu1

!  2) Si nu2z est present :
!     * Calculer un nouveau nume_ddl nu2z correspondant au nume_ddl nu1z mais avec
!       la numerotation 'RCMK'.
!     * Retourner egalement l'objet perm qui etablit la permutation entre nu1z et nu2z.
!     nu2z est cree sur la base volatile ('V')
!     perm est cree sur la base 'basp'
!
! --------------------------------------------------------------------------------------------------
!
! (o) In/Jxvar  nu1z  : sd_nume_ddl
! (f) In/Jxout  nu2z  : sd_nume_ddl
! (f) In/Jxout  perm  : K24 : OJB V I
! (f) In        basp  : K1 : base pour la creation de perm
!
! --------------------------------------------------------------------------------------------------
!
    character(len=14) :: nu1, nu2, nutrav
    character(len=8) :: nogd
    character(len=1) :: bas1
    integer :: nb_ligr,k,neq2,ncmp1,ncmp2,nbno1,nbno2,iad1,iad2,icmp,ieq1,ieq2,nbec
    integer :: nec1,nec2,n1,n2,ino,neq,iexi,ifm,niv
    character(len=24), pointer :: list_ligr(:) => null()
    character(len=24), pointer :: refn(:) => null()
    character(len=19) :: nomligr
    character(len=8) :: modelo, nomres
    character(len=16) :: nomcom, typres
    logical :: non_renum, limpr
    integer, pointer :: prno1(:) => null()
    integer, pointer :: prno2(:) => null()
    integer, pointer :: nueq1(:) => null()
    integer, pointer :: nueq2(:) => null()
    integer, pointer :: m2lc(:) => null()
    integer, pointer :: lc2m(:) => null()
    character(len=16), SAVE :: nomcom_sav=' ', nomres_sav=' '
    integer, SAVE :: neq_sav=0

! --------------------------------------------------------------------------------------------------
    call jemarq()
    nu1=nu1z
    call dismoi('NB_EQUA', nu1, 'NUME_DDL', repi=neq)
    call getres(nomres, typres, nomcom)

!   -- On ne veut pas ecrire les informations de numerotation (RCMK, ...)
!      a chaque resolution (si INFO=1).
    limpr=.true.
    call infniv(ifm, niv)
    if (nomcom.eq.nomcom_sav .and. nomres.eq.nomres_sav) then
        if (neq.eq.neq_sav .and. niv.eq.1) limpr=.false.
    endif
    neq_sav=neq
    nomres_sav=nomres
    nomcom_sav=nomcom

    if (.not.limpr) call infmue()

    if (.not.present(nu2z)) then
        call jelira(nu1//'.SMOS.SMDE', 'CLAS', cval=bas1)
        call jedetr(nu1//'.SLCS.M2LC')
        call jedetr(nu1//'.SLCS.LC2M')
        call wkvect(nu1//'.SLCS.M2LC', bas1//' V I', neq, vi=m2lc)
        call wkvect(nu1//'.SLCS.LC2M', bas1//' V I', neq, vi=lc2m)
    else
        nu2=nu2z
        ASSERT(present(perm))
        ASSERT(present(basp))
        call jedetr(perm)
        call wkvect(perm, basp//' V I', neq, vi=m2lc)
    endif


!   -- On ne renumerote pas (avec RCMK) si :
!       * C'est un NUME_DDL_GENE (pas de noeuds)
!       * On est dans la commande MACR_ELEM_STAT (RCMK a deja ete fait)
!       * C'est un le NUME_DDL d'une matrice "reduite" avec ELIM_LAGR,
!         (car on a supprime certains ddls)
!   -------------------------------------------------------------------
    non_renum=(nomcom.eq.'MACR_ELEM_STAT')
    call jenuno(jexnum(nu1//'.NUME.LILI', 1), nomligr)
    non_renum=non_renum.or.(nomligr.eq.'&SOUSSTR')
    call jeveuo(nu1//'.NUME.REFN','L',vk24=refn)
    non_renum=non_renum.or.(refn(4).eq.'ELIM_LAGR')
    if (non_renum) then
        if (present(nu2z)) then
            call copisd('NUME_DDL', 'V', nu1, nu2)
            do k=1,neq
                m2lc(k)=k
            enddo
        else
            do k=1,neq
                m2lc(k)=k
                lc2m(k)=k
            enddo
        endif
        goto 999
    endif


!   -- Si on est dans la commande CALC_ERREUR, il faut utiliser l'argument modelocz de nueffe :
!   -------------------------------------------------------------------------------------------
    if (nomcom.eq.'CALC_ERREUR') then
        modelo='DDL_NOZ1'
    else
        modelo=' '
    endif


!   -- Renumerotation RCMK :
!   ---------------------------------------------------------------
    if (present(nu2z)) then
        nutrav=nu2
    else
        nutrav='&&ldlt_RENUM.N'
    endif
    call copisd('NUME_EQUA', 'V', nu1//'.NUME', nutrav//'.NUME')

!   -- il faut ignorer .LILI(1) -> '&MAILLA' :
    call jelira(nu1//'.NUME.LILI', 'NOMUTI', nb_ligr)
    ASSERT(nb_ligr.ge.2)
    nb_ligr=nb_ligr-1
    AS_ALLOCATE(vk24=list_ligr,size=nb_ligr)
    list_ligr(:)=' '
    do k=1,nb_ligr
        call jenuno(jexnum(nu1//'.NUME.LILI', 1+k), nomligr)
        list_ligr(k)=nomligr
    enddo
    call jedetr(nutrav//'     .ADNE')
    call jedetr(nutrav//'     .ADLI')
    call nueffe(nb_ligr, list_ligr, 'VV', nutrav, 'RCMK',modelocz=modelo)
    AS_DEALLOCATE(vk24=list_ligr)


!   -- Pour etablir la correspondance entre les deux numerotations,
!      il faut comparer les objets .PRNO et .NUEQ :
!   ---------------------------------------------------------------
    call dismoi('NB_EQUA', nutrav, 'NUME_DDL', repi=neq2)
    ASSERT(neq.eq.neq2)

    call dismoi('NOM_GD', nu1, 'NUME_DDL', repk=nogd)
    call dismoi('NB_EC', nogd, 'GRANDEUR', repi=nec1)
    call dismoi('NOM_GD', nutrav, 'NUME_DDL', repk=nogd)
    call dismoi('NB_EC', nogd, 'GRANDEUR', repi=nec2)
    ASSERT(nec1.eq.nec2)
    nbec=nec1

    call jelira(nu1//'.NUME.PRNO', 'NMAXOC', n1)
    call jelira(nutrav//'.NUME.PRNO', 'NMAXOC', n2)
    ASSERT(n1.eq.n2)

    call jeveuo(nu1//'.NUME.NUEQ', 'L', vi=nueq1)
    call jeveuo(nutrav//'.NUME.NUEQ', 'L', vi=nueq2)

    do k=1,n1
        call jelira(jexnum(nu1//'.NUME.PRNO',k), 'LONMAX', iexi)
        if (iexi.eq.0) cycle
        call jeveuo(jexnum(nu1//'.NUME.PRNO',k), 'L', vi=prno1)
        call jeveuo(jexnum(nutrav//'.NUME.PRNO',k), 'L', vi=prno2)
        nbno1 = size(prno1)/(nbec+2)
        nbno2 = size(prno2)/(nbec+2)
        ASSERT(nbno1.eq.nbno2)
        do ino=1,nbno1
            ncmp1=prno1((ino-1)*(2+nbec)+2)
            ncmp2=prno2((ino-1)*(2+nbec)+2)
            ASSERT(ncmp1.eq.ncmp2)
            iad1=prno1((ino-1)*(2+nbec)+1)
            iad2=prno2((ino-1)*(2+nbec)+1)
            do icmp=1,ncmp1
                ieq1=nueq1(iad1-1+icmp)
                ieq2=nueq2(iad2-1+icmp)
                ASSERT(ieq1.ge.1 .and. ieq1.le.neq)
                ASSERT(ieq2.ge.1 .and. ieq2.le.neq)
                m2lc(ieq1)=ieq2
            enddo
        enddo
    enddo

    do k=1,neq
        ASSERT(m2lc(k).ge.1 .and. m2lc(k).le.neq)
    enddo

    if (.not.present(nu2z)) then
        do k=1,neq
            lc2m(m2lc(k))=k
        enddo
        call detrsd('NUME_EQUA',nutrav//'.NUME')
    else
        call promor(nutrav, 'V')
    endif


999 continue

    if (.not.limpr) call infbav()
    call jedema()
end subroutine
