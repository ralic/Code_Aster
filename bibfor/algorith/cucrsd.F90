subroutine cucrsd(noma, numedd, deficu, resocu)
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
    implicit none
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
    character(len=8) :: noma
    character(len=24) :: numedd
    character(len=24) :: deficu, resocu
!
! ----------------------------------------------------------------------
!
! ROUTINE LIAISON_UNILATER
!
! CREATION DES STRUCTURES DE DONNEES NECESSAIRES AU TRAITEMENT
! DES LIAISONS UNILATERALES
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  NUMEDD : NUMEROTATION
! OUT DEFICU : SD DE DEFINITION (ISSUE D'AFFE_CHAR_MECA)
! OUT RESOCU : SD DE TRAITEMENT NUMERIQUE
!
!
!
!
    integer :: ifm, niv
    character(len=8) ::  cmp, nomno
    integer :: ino, iddl, cddl
    character(len=24) :: ddlco, atmu, apjeu
    character(len=24) :: valk(2)
    integer :: jddl, jatmu, japjeu
    integer :: nnocu, nbcmp, ncmpg, numno
    integer :: icmp
    integer :: ier, iret, jdecal, i, neq
    character(len=24) :: cmpgcu, lisnoe, poinoe, apcoef, nomnoe, nomcmp
    integer :: jcmpg, jnoe, jpoi, japcoe, jnomno, jnomcm
    character(len=24) :: noeuma, coefmu
    integer :: jcoef
    character(len=24) :: cm1a, coco, liac, mu, delt0, delta, liot
    integer :: jcoco, jliac, jmu, jdelt0, jdelta, jliot
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- INITIALISATIONS
!
    noeuma = noma // '.NOMNOE'
    call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CREATION DE LA SD RESULTAT'//&
        ' LIAISON_UNILATERALE'
    endif
!
! --- VECTEUR AT.MU (FORCES NODALES)
!
    atmu = resocu(1:14)//'.ATMU'
    call jeexin(atmu, iret)
    if (iret .eq. 0) then
        call wkvect(atmu, 'V V R', neq, jatmu)
    endif
!
! --- NOMBRE TOTAL DE DDLS ET NOMBRE TOTAL DE NOEUDS
!
    nnocu = cudisi(deficu,'NNOCU')
    ncmpg = cudisi(deficu,'NCMPG')
!
! --- SD D'INFOS
!
    lisnoe = deficu(1:16)//'.LISNOE'
    poinoe = deficu(1:16)//'.POINOE'
    cmpgcu = deficu(1:16)//'.CMPGCU'
    call jeveuo(cmpgcu, 'L', jcmpg)
    call jeveuo(lisnoe, 'L', jnoe)
    call jeveuo(poinoe, 'L', jpoi)
!
! --- VECTEUR CONTENANT LES NUMEROS DES DDL DU MEMBRE DE GAUCHE
!
    ddlco = resocu(1:14)//'.APDDL'
    call wkvect(ddlco, 'V V I', 30*nnocu, jddl)
!
    nomnoe = resocu(1:14)//'.NOMNOE'
    nomcmp = resocu(1:14)//'.NOMCMP'
    call wkvect(nomnoe, 'V V K8', ncmpg, jnomno)
    call wkvect(nomcmp, 'V V K8', ncmpg, jnomcm)
!
    zi(jddl) = 0
    iddl = 1
!
    do 100 ino = 1, nnocu
!
        numno = zi(jnoe+ino-1)
        call jenuno(jexnum(noeuma, numno), nomno)
        nbcmp = zi(jpoi+ino) - zi(jpoi+ino-1)
        jdecal = zi(jpoi+ino-1)
!
        do 200 icmp = jdecal, jdecal+nbcmp-1
!
            cmp = zk8(jcmpg-1+icmp)
!
            call posddl('NUME_DDL', numedd, nomno, cmp, numno,&
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
200     continue
100 end do
    if ((iddl-1) .ne. ncmpg) then
        ASSERT(.false.)
    endif
!
! --- VECTEUR COEFFICIENTS DES DDLS DE GAUCHE
!
    apcoef = resocu(1:14)// '.APCOEF'
    call wkvect(apcoef, 'V V R', 30*nnocu, japcoe)
!
! --- VECTEUR PSEUDO-JEU
!
    apjeu = resocu(1:14)//'.APJEU'
    call jeexin(apjeu, iret)
    if (iret .eq. 0) then
        call wkvect(apjeu, 'V V R', nnocu, japjeu)
    endif
!
! --- VECTEURS DE TRAVAIL
!
    coco = resocu(1:14)//'.COCO'
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
    liac = resocu(1:14)//'.LIAC'
    call jeexin(liac, iret)
    if (iret .eq. 0) then
        call wkvect(liac, 'V V I', 3*nnocu+1, jliac)
    endif
!
! --- VECTEUR MU
!
    mu = resocu(1:14)//'.MU'
    call jeexin(mu, iret)
    if (iret .eq. 0) then
        call wkvect(mu, 'V V R', 6*nnocu, jmu)
    endif
!
! --- VECTEUR COEFMU
!
    coefmu = resocu(1:14)//'.COEFMU'
    call jeexin(coefmu, iret)
    if (iret .eq. 0) then
        call wkvect(coefmu, 'V V R', nnocu, jcoef)
    endif
!
! --- VECTEUR DELTA ET DELTA0
!
    delt0 = resocu(1:14)//'.DEL0'
    call jeexin(delt0, iret)
    if (iret .eq. 0) then
        call wkvect(delt0, 'V V R', neq, jdelt0)
    endif
    delta = resocu(1:14)//'.DELT'
    call jeexin(delta, iret)
    if (iret .eq. 0) then
        call wkvect(delta, 'V V R', neq, jdelta)
    endif
!
! --- VECTEUR POUR PIVOTS NULS
!
    liot = resocu(1:14)//'.LIOT'
    call jeexin(liot, ier)
    if (ier .eq. 0) then
        call wkvect(liot, 'V V I', 4*nnocu+4, jliot)
    endif
!
! --- MATRICE CM1AT
!
    cm1a = resocu(1:14)//'.CM1A'
    call jecrec(cm1a, 'V V R', 'NU', 'DISPERSE', 'CONSTANT',&
                ncmpg)
    call jeecra(cm1a, 'LONMAX', neq)
    do 40 i = 1, ncmpg
        call jecroc(jexnum(cm1a, i))
 40 end do
!
! --- MATRICE DE LA LIAISON_UNILATERALE ACM1AT
!
    call cfcrma(ncmpg, noma, resocu)
!
!
! ----------------------------------------------------------------------
    call jedema()
end subroutine
