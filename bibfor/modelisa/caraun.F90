subroutine caraun(char, motfac, nzocu, nbgdcu, coefcu,&
                  compcu, multcu, ntcmp)
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
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=8) :: char
    character(len=16) :: motfac
    integer :: nzocu, ntcmp
    character(len=24) :: nbgdcu
    character(len=24) :: coefcu
    character(len=24) :: compcu
    character(len=24) :: multcu
!
! ----------------------------------------------------------------------
!
! ROUTINE LIAISON_UNILATERALE (LECTURE)
!
! LIRE LES CARACTERISTIQUES DE LA LIAISON UNILATERALE
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM DU CONCEPT CHARGE
! IN  MOTFAC : MOT_CLEF FACTEUR POUR LIAISON UNILATERALE
! IN  NZOCU  : NOMBRE DE ZONES DE LIAISON_UNILATERALE
! IN  NBGDCU : NOM JEVEUX DE LA SD INFOS POINTEURS GRANDEURS
!       ZI(JNBGD+IOCC-1): INDICE DEBUT DANS LISTE DES NOMS DES GRANDEURS
!                       POUR ZONE IOCC
!       ZI(JNBGD+IOCC) - ZI(JNBGD+IOCC-1): NOMBRE DE GRANDEURS DE LA
!                       ZONE IOCC
! IN  COEFCU : NOM JEVEUX DE LA SD CONTENANT LES COEFFICIENTS DES
!              GRANDEURS DE MEMBRE DE DROITE
!              VECTEUR TYPE ZR OU ZK8 SUIVANT FONREE
!       Z*(JCOEF+IOCC-1): VALEUR OU NOM FONCTION DU MEMBRE DE DROITE
! IN  COMPCU : NOM JEVEUX DE LA SD CONTENANT LES GRANDEURS DU MEMBRE
!              DE GAUCHE
!              LONGUEUR = ZI(JDUME+3)
!              INDEXE PAR NBGDCU:
!       ZI(JNBGD+IOCC-1): INDEX DEBUT POUR ZONE IOCC
!       ZI(JDUME+2*(IOCC-1)+5) = ZI(JNBGD+IOCC)-ZI(JNBGD+IOCC-1):
!                         NOMBRE GRANDEURS A GAUCHE POUR ZONE IOCC
!       ZK8(JCMPG-1+INDEX+ICMP-1): NOM ICMP-EME GRANDEUR
! IN  MULTCU : NOM JEVEUX DE LA SD CONTENANT LES COEF DU MEMBRE
!              DE GAUCHE
!              VECTEUR TYPE ZR OU ZK8 SUIVANT FONREE
!              MEME ACCES QUE COMPCU
! OUT NTCMP  : NOMBRE TOTAL DE COMPOSANTES SUR TOUTES LES ZONES
!
!
!
!
!
    integer :: zmax
    parameter     (zmax = 30)
    character(len=8) :: cmpgd(zmax), k8bid, ccoef, ccmult(zmax)
    integer :: noc, nbcmp, nbcmul
    integer :: izone, icmp, iform
    character(len=24) :: defico
    character(len=24) :: paraci
    integer :: jparci
    integer :: jcoef, jnbgd, jcmpg, jmult
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    iform = 4
    ntcmp = 0
    defico = char(1:8)//'.CONTACT'
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    paraci = defico(1:16)//'.PARACI'
    call jeveuo(paraci, 'E', jparci)
!
! --- LA FORMULATION (UNIQUE !)
!
    zi(jparci+4-1) = iform
!
! --- METHODE: CONTRAINTES ACTIVES
!
    zi(jparci+17-1) = 0
!
! --- PARAMETRES
!
    zi(jparci+3-1) = 10
!
! --- MEMBRE DE DROITE DE L'INEGALITE
!
    coefcu = '&&CARAUN.COEFCU'
    call wkvect(coefcu, 'V V K8', nzocu, jcoef)
    do 1001 izone = 1, nzocu
        call getvid(motfac, 'COEF_IMPO', iocc=izone, scal=ccoef, nbret=noc)
        zk8(jcoef-1+izone) = ccoef
1001  end do
!
! --- MEMBRE DE GAUCHE DE L'INEGALITE
! --- COMPTAGE PREALABLE DU NOMBRE DE DDLS
!
    call wkvect(nbgdcu, 'V V I', nzocu+1, jnbgd)
    zi(jnbgd) = 1
    ntcmp = 0
    do 1002 izone = 1, nzocu
        call getvtx(motfac, 'NOM_CMP', iocc=izone, scal=k8bid, nbret=nbcmp)
        call getvid(motfac, 'COEF_MULT', iocc=izone, scal=k8bid, nbret=nbcmul)
        if (nbcmp .ne. nbcmul) then
            call u2mess('F', 'UNILATER_42')
        endif
        nbcmp = abs(nbcmp)
        nbcmul = abs(nbcmul)
        ntcmp = ntcmp + nbcmp
        if (ntcmp .gt. zmax) then
            call u2mess('F', 'UNILATER_43')
        endif
        zi(jnbgd+izone) = zi(jnbgd+izone-1) + nbcmp
1002  end do
!
! --- MEMBRE DE GAUCHE DE L'INEGALITE: CREATION DES OBJETS
!
    call wkvect(compcu, 'V V K8', ntcmp, jcmpg)
    call wkvect(multcu, 'V V K8', ntcmp, jmult)
!
! --- MEMBRE DE GAUCHE DE L'INEGALITE: REMPLISSAGE DES OBJETS
!
    do 2000 izone = 1, nzocu
        nbcmp = zi(jnbgd+izone) - zi(jnbgd+izone-1)
        call getvtx(motfac, 'NOM_CMP', iocc=izone, nbval=nbcmp, vect=cmpgd,&
                    nbret=noc)
        call getvid(motfac, 'COEF_MULT', iocc=izone, nbval=nbcmp, vect=ccmult,&
                    nbret=noc)
        do 11 icmp = 1, nbcmp
            zk8(jcmpg-1+zi(jnbgd+izone-1)+icmp-1) = cmpgd(icmp)
            zk8(jmult-1+zi(jnbgd+izone-1)+icmp-1) = ccmult(icmp)
11      continue
2000  end do
!
    call jedema()
!
end subroutine
