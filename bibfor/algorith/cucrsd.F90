subroutine cucrsd(mesh, nume_dof, ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfcrma.h"
#include "asterfort/cudisi.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/posddl.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
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
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=8), intent(in) :: mesh
    character(len=24), intent(in) :: nume_dof
    type(NL_DS_Contact), intent(in) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! Unilateral conditions - Solve
!
! Prepare unilateral conditions solving datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  nume_dof         : name of numbering object (NUME_DDL)
! In  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdunil_defi, sdunil_solv
    integer :: ifm, niv
    character(len=8) :: cmp, nomno
    integer :: ino, iddl, cddl
    character(len=24) :: ddlco, atmu, apjeu
    character(len=24) :: valk(2)
    integer :: jddl, jatmu, japjeu
    integer :: nnocu, nbcmp, ncmpg, numno
    integer :: icmp
    integer :: ier, iret, jdecal, i, neq
    character(len=24) :: cmpgcu, lisnoe, poinoe, apcoef, nomnoe, nomcmp
    integer :: jcmpg, jnoe, jpoi, japcoe, jnomno, jnomcm
    character(len=24) :: mesh_nomnoe, coefmu
    integer :: jcoef
    character(len=24) :: cm1a, coco, liac, mu, delt0, delta, liot
    integer :: jcoco, jliac, jmu, jdelt0, jdelta, jliot
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CREATION DE LA SD RESULTAT LIAISON_UNILATERALE'
    endif
!
! --- INITIALISATIONS
!
    sdunil_defi = ds_contact%sdunil_defi
    sdunil_solv = ds_contact%sdunil_solv
    mesh_nomnoe = mesh // '.NOMNOE'
    call dismoi('NB_EQUA', nume_dof, 'NUME_DDL', repi=neq)
!
! --- VECTEUR AT.MU (FORCES NODALES)
!
    atmu = sdunil_solv(1:14)//'.ATMU'
    call jeexin(atmu, iret)
    if (iret .eq. 0) then
        call wkvect(atmu, 'V V R', neq, jatmu)
    endif
!
! --- NOMBRE TOTAL DE DDLS ET NOMBRE TOTAL DE NOEUDS
!
    nnocu = cudisi(sdunil_defi,'NNOCU')
    ncmpg = cudisi(sdunil_defi,'NCMPG')
!
! --- SD D'INFOS
!
    lisnoe = sdunil_defi(1:16)//'.LISNOE'
    poinoe = sdunil_defi(1:16)//'.POINOE'
    cmpgcu = sdunil_defi(1:16)//'.CMPGCU'
    call jeveuo(cmpgcu, 'L', jcmpg)
    call jeveuo(lisnoe, 'L', jnoe)
    call jeveuo(poinoe, 'L', jpoi)
!
! --- VECTEUR CONTENANT LES NUMEROS DES DDL DU MEMBRE DE GAUCHE
!
    ddlco = sdunil_solv(1:14)//'.APDDL'
    call wkvect(ddlco, 'V V I', 30*nnocu, jddl)
!
    nomnoe = sdunil_solv(1:14)//'.NOMNOE'
    nomcmp = sdunil_solv(1:14)//'.NOMCMP'
    call wkvect(nomnoe, 'V V K8', ncmpg, jnomno)
    call wkvect(nomcmp, 'V V K8', ncmpg, jnomcm)
!
    zi(jddl) = 0
    iddl = 1
!
    do ino = 1, nnocu
!
        numno = zi(jnoe+ino-1)
        call jenuno(jexnum(mesh_nomnoe, numno), nomno)
        nbcmp = zi(jpoi+ino) - zi(jpoi+ino-1)
        jdecal = zi(jpoi+ino-1)
!
        do icmp = jdecal, jdecal+nbcmp-1
!
            cmp = zk8(jcmpg-1+icmp)
!
            call posddl('NUME_DDL', nume_dof, nomno, cmp, numno,&
                        cddl)
            if (cddl .eq. 0) then
                valk (1) = nomno
                valk (2) = cmp
                call utmess('F', 'UNILATER_75', nk=2, valk=valk)
            else
                zk8(jnomno+iddl-1) = nomno
                zk8(jnomcm+iddl-1) = cmp
                zi(jddl+iddl) = cddl
                iddl = iddl+1
            endif
        end do
    end do
    if ((iddl-1) .ne. ncmpg) then
        ASSERT(.false.)
    endif
!
! --- VECTEUR COEFFICIENTS DES DDLS DE GAUCHE
!
    apcoef = sdunil_solv(1:14)// '.APCOEF'
    call wkvect(apcoef, 'V V R', 30*nnocu, japcoe)
!
! --- VECTEUR PSEUDO-JEU
!
    apjeu = sdunil_solv(1:14)//'.APJEU'
    call jeexin(apjeu, iret)
    if (iret .eq. 0) then
        call wkvect(apjeu, 'V V R', nnocu, japjeu)
    endif
!
! --- VECTEURS DE TRAVAIL
!
    coco = sdunil_solv(1:14)//'.COCO'
    call jeexin(coco, ier)
    if (ier .eq. 0) then
        call wkvect(coco, 'V V I', 8, jcoco)
    endif
    zi(jcoco ) = 3
    zi(jcoco+1) = 0
    zi(jcoco+2) = 0
    zi(jcoco+3) = 0
    zi(jcoco+4) = 1
    zi(jcoco+5) = 0
    zi(jcoco+6) = 0
    zi(jcoco+7) = 0
!
    liac = sdunil_solv(1:14)//'.LIAC'
    call jeexin(liac, iret)
    if (iret .eq. 0) then
        call wkvect(liac, 'V V I', 3*nnocu+1, jliac)
    endif
!
! --- VECTEUR MU
!
    mu = sdunil_solv(1:14)//'.MU'
    call jeexin(mu, iret)
    if (iret .eq. 0) then
        call wkvect(mu, 'V V R', 6*nnocu, jmu)
    endif
!
! --- VECTEUR COEFMU
!
    coefmu = sdunil_solv(1:14)//'.COEFMU'
    call jeexin(coefmu, iret)
    if (iret .eq. 0) then
        call wkvect(coefmu, 'V V R', nnocu, jcoef)
    endif
!
! --- VECTEUR DELTA ET DELTA0
!
    delt0 = sdunil_solv(1:14)//'.DEL0'
    call jeexin(delt0, iret)
    if (iret .eq. 0) then
        call wkvect(delt0, 'V V R', neq, jdelt0)
    endif
    delta = sdunil_solv(1:14)//'.DELT'
    call jeexin(delta, iret)
    if (iret .eq. 0) then
        call wkvect(delta, 'V V R', neq, jdelta)
    endif
!
! --- VECTEUR POUR PIVOTS NULS
!
    liot = sdunil_solv(1:14)//'.LIOT'
    call jeexin(liot, ier)
    if (ier .eq. 0) then
        call wkvect(liot, 'V V I', 4*nnocu+4, jliot)
    endif
!
! --- MATRICE CM1AT
!
    cm1a = sdunil_solv(1:14)//'.CM1A'
    call jecrec(cm1a, 'V V R', 'NU', 'DISPERSE', 'CONSTANT',&
                ncmpg)
    call jeecra(cm1a, 'LONMAX', neq)
    do i = 1, ncmpg
        call jecroc(jexnum(cm1a, i))
    end do
!
! --- MATRICE DE LA LIAISON_UNILATERALE ACM1AT
!
    call cfcrma(ncmpg, mesh, sdunil_solv)
!
    call jedema()
end subroutine
