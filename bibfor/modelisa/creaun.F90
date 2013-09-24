subroutine creaun(char, noma, nomo, nzocu, nnocu,&
                  lisnoe, poinoe, nbgdcu, coefcu, compcu,&
                  multcu)
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
#include "asterfort/exiscp.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: char
    character(len=8) :: noma
    character(len=8) :: nomo
    integer :: nzocu, nnocu
    character(len=24) :: lisnoe
    character(len=24) :: poinoe
    character(len=24) :: nbgdcu
    character(len=24) :: coefcu
    character(len=24) :: compcu
    character(len=24) :: multcu
!
! ----------------------------------------------------------------------
!
! ROUTINE LIAISON_UNILATERALE (CREATION SD)
!
! CONSTRUCTION FINALE DES VECTEURS ON OUBLIE LE CONCEPT DE ZONES
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM DU CONCEPT CHARGE
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  NZOCU  : NOMBRE DE ZONES
! IN  NNOCU  : NOMBRE DE NOEUDS
! IN  POINOE : NOM DE L'OBJET CONTENANT LE VECTEUR D'INDIRECTION
!               DES NOEUDS
! IN  LISNOE : NOM DE L'OBJET CONTENANT LES NOEUDS
! IN  NBGDCU : NOM JEVEUX DE LA SD INFOS POINTEURS GRANDEURS DU MEMBRE
!              DE GAUCHE
! IN  COEFCU : NOM JEVEUX DE LA SD CONTENANT LES VALEURS DU MEMBRE
!              DE DROITE
! IN  COMPCU : NOM JEVEUX DE LA SD CONTENANT LES GRANDEURS DU MEMBRE
!              DE GAUCHE
! IN  MULTCU : NOM JEVEUX DE LA SD CONTENANT LES COEFFICIENTS DU MEMBRE
!              DE GAUCHE
!
!
!
!
!
    integer :: nbgau
    character(len=24) :: deficu
    integer :: jmult, jnoe, jpoi, jnbgd
    integer :: jcoed, jcoeg, jcompg, jcoef, jncmp
    integer :: ino, icmp, izone
    character(len=24) :: noeucu, noeuma
    character(len=24) :: valk(2)
    integer :: jnoeu, jindir
    integer :: numnd, exist(1), nbsup
    integer :: nbno, nbcmp
    integer :: jdebcp, jdebnd
    character(len=8) :: cmp, k8bla, nomno
    integer :: cptd, ncmpg, cptnd
    character(len=24) :: cmpgcu, ndimcu, coegcu, coedcu, poincu
    integer :: jcmpg, jdim, jcoefg, jcoefd, jpoin
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
! --- INITIALISATIONS
!
    deficu = char(1:8)//'.UNILATE'
    noeuma = noma // '.NOMNOE'
    k8bla = ' '
    call jeveuo(multcu, 'L', jmult)
    call jeveuo(poinoe, 'L', jpoi)
    call jeveuo(lisnoe, 'L', jnoe)
    call jeveuo(nbgdcu, 'L', jnbgd)
    call jeveuo(compcu, 'L', jncmp)
    call jeveuo(coefcu, 'L', jcoef)
!
! --- CALCUL DU NOMBRE TOTAL DE GRANDEURS A GAUCHE
!
    nbgau = 0
    do 20 izone = 1, nzocu
        nbno = zi(jpoi+izone) - zi(jpoi+izone-1)
        nbcmp = zi(jnbgd+izone) - zi(jnbgd+izone-1)
        nbgau = nbgau + nbno*nbcmp
20  end do
!
! --- CREATION DES VECTEURS DEFINITIFS
!
    noeucu = deficu(1:16)//'.LISNOE'
    call wkvect(noeucu, 'G V I', nnocu, jnoeu)
!
! --- CREATION DES VECTEURS TEMPORAIRES
!
    call wkvect('&&CREAUN.INDIR', 'V V I', nnocu+1, jindir)
    call wkvect('&&CREAUN.CMPG', 'V V K8', nbgau, jcompg)
    call wkvect('&&CREAUN.COEFG', 'V V K8', nbgau, jcoeg)
    call wkvect('&&CREAUN.COEFD', 'V V K8', nnocu, jcoed)
    zi(jindir) = 1
!
! ---
!
    cptnd = 1
    cptd = 1
    ncmpg = 1
!
    do 1000 izone = 1, nzocu
!
        nbno = zi(jpoi+izone) - zi(jpoi+izone-1)
        jdebnd = zi(jpoi+izone-1)
        nbcmp = zi(jnbgd+izone) - zi(jnbgd+izone-1)
        jdebcp = zi(jnbgd+izone-1)
!
        do 2000 ino = 1, nbno
!
            numnd = zi(jnoe-1+jdebnd+ino-1)
            nbsup = 0
!
            do 3000 icmp = 1, nbcmp
!
                cmp = zk8(jncmp-1+jdebcp+icmp-1)
!
                call exiscp(cmp, k8bla, nomo, 1, 'NUM',&
                            k8bla, [numnd], exist)
!
                if (exist(1) .eq. 1) then
                    if (niv .ge. 2) then
                        call jenuno(jexnum(noeuma, numnd), nomno)
                        valk (1) = nomno
                        valk (2) = cmp
                        call utmess('I', 'UNILATER_58', nk=2, valk=valk)
                    endif
                    zk8(jcompg-1+ncmpg) = cmp
                    zk8(jcoeg-1+ncmpg) = zk8(jmult-1+jdebcp+icmp-1)
                    ncmpg = ncmpg + 1
                else
                    nbsup = nbsup + 1
                    call jenuno(jexnum(noeuma, numnd), nomno)
                    valk (1) = nomno
                    valk (2) = cmp
                    call utmess('I', 'UNILATER_75', nk=2, valk=valk)
                endif
!
3000          continue
!
            zi(jnoeu-1+cptnd) = numnd
            zk8(jcoed-1+cptd) = zk8(jcoef+izone-1)
            zi(jindir+cptnd) = zi(jindir+cptnd-1) + nbcmp - nbsup
!
            cptd = cptd + 1
            cptnd = cptnd + 1
!
2000      continue
1000  end do
!
    cptd = cptd - 1
    cptnd = cptnd - 1
    ncmpg = ncmpg - 1
!
    ASSERT(cptd.eq.nnocu)
    ASSERT(cptnd.eq.nnocu)
!
! --- QUELQUES INFOS DIMENSIONS
!
    ndimcu = deficu(1:16)//'.NDIMCU'
    call jeveuo(ndimcu, 'E', jdim)
    zi(jdim) = nnocu
    zi(jdim+1) = ncmpg
!
! --- LISTE DES POINTEURS DES NOEUDS
!
    poincu = deficu(1:16)//'.POINOE'
    call wkvect(poincu, 'G V I', nnocu+1, jpoin)
    do 4000 ino = 1, nnocu+1
        zi(jpoin-1+ino) = zi(jindir-1+ino)
4000  end do
!
! --- LISTE DES NOMS DE COMPOSANTES A GAUCHE
!
    cmpgcu = deficu(1:16)//'.CMPGCU'
    call wkvect(cmpgcu, 'G V K8', ncmpg, jcmpg)
    do 4001 icmp = 1, ncmpg
        zk8(jcmpg-1+icmp) = zk8(jcompg-1+icmp)
4001  end do
!
! --- LISTE DES COEFFICIENTS A DROITE ET A GAUCHE
!
    coegcu = deficu(1:16)//'.COEFG'
    coedcu = deficu(1:16)//'.COEFD'
    call wkvect(coegcu, 'G V K8', ncmpg, jcoefg)
    call wkvect(coedcu, 'G V K8', nnocu, jcoefd)
!
    do 4003 icmp = 1, ncmpg
        zk8(jcoefg-1+icmp) = zk8(jcoeg-1+icmp)
4003  end do
!
    do 4004 icmp = 1, cptd
        zk8(jcoefd-1+icmp) = zk8(jcoed-1+icmp)
4004  end do
!
! --- NETTOYAGE
!
    call jedetr('&&CREAUN.INDIR')
    call jedetr('&&CREAUN.CMPG')
    call jedetr('&&CREAUN.COEFG')
    call jedetr('&&CREAUN.COEFD')
!
! ======================================================================
    call jedema()
!
end subroutine
