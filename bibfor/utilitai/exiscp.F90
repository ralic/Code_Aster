subroutine exiscp(nomcmp, char, modele, nbnd, typend,&
                  nomnd, numnd, resu)
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
!
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
!
    character(len=8) :: nomcmp
    character(len=8) :: char
    character(len=8) :: modele
    integer :: nbnd
    character(len=3) :: typend
    character(len=8) :: nomnd(*)
    integer :: numnd(*)
    integer :: resu(*)
!
! ----------------------------------------------------------------------
!  CETTE ROUTINE DIT SI LA(LES) COMPOSANTE(S) EXISTE(NT) SUR UN NOEUD
! ----------------------------------------------------------------------
!
!CC
!
! IN  NOMCMP  : NOM DU DDL
! IN  CHAR    : CONCEPT RESULTANT DU CHARGEMENT
!                 SI CHAR = ' ' -> ON PREND LE MODELE DANS MODELE
! IN  MODELE  : NOM DU MODELE SI CHAR = ' '
! IN  NBND    : NOMBRE DE NOEUDS SUR LESQUELS ON CHECKE LE DDL
! IN  TYPEND  : LA LISTE DE NOEUDS EST FAITE
!               - DE LEUR NOM SI TYPEND='NOM'
!               - DE LEUR NUMERO SI TYPEND='NUM'
! IN  NOMND   : LISTE DES NOEUDS (NOMS)
! IN  NUMND   : LISTE DES NOEUDS (NUMEROS)
! OUT RESU    : LISTE D'INTEGER CONTENANT LE RESULTAT
!                 POUR CHAQUE NOEUD: 1 SI LE DDL EXISTE EN CE NOEUD
!                                    0 SINON
!
!
!
!
    integer :: jnom, jnoma, jprnm, jexis
    integer :: i, icmp, ino
    integer :: nbcmp, nmocl, nbec
    parameter    (nmocl=300)
    character(len=8) :: nomddl(nmocl), noma, mod
    character(len=16) :: pheno
    character(len=19) :: ligrmo
    character(len=24) :: nomnoe
    character(len=8) :: nomgd
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    call dismoi('PHENOMENE', modele, 'MODELE', repk=pheno)
    call dismoi('NOM_GD', pheno, 'PHENOMENE', repk=nomgd)
!
! --- NOMBRE D'ENTIERS CODES POUR LA GRANDEUR
!
    call dismoi('NB_EC', nomgd, 'GRANDEUR', repi=nbec)
    ASSERT(nbec.le.10)
!
! --- RECUPERATION DES NOMS DES DDLS DISPONIBLES POUR UNE GRANDEUR
!
    call jeexin(jexnom('&CATA.GD.NOMCMP', nomgd), jexis)
    if (jexis .eq. 0) then
        call utmess('F', 'UTILITAI_73')
    endif
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomgd), 'L', jnom)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomgd), 'LONMAX', nbcmp)
!
! --- NOMBRE DE DDL POUR CETTE GRANDEUR
!
    nbcmp = nbcmp - 1
!
! --- TROP DE DDLS POUR CETTE GRANDEUR
!
    ASSERT(nbcmp.le.nmocl)
!
! --- NOM DES DDLS POUR CETTE GRANDEUR
!
    do 10 i = 1, nbcmp
        nomddl(i) = zk8(jnom-1+i)
 10 end do
!
! --- INDICE DU DDL DANS LE TABLEAU NOMCMP
!
    icmp = indik8(nomddl,nomcmp(1:8),1,nbcmp)
!
! --- DDL INEXISTANT POUR CETTE GRANDEUR
!
    if (icmp .eq. 0) then
        call utmess('F', 'UTILITAI_74')
    endif
!
! --- NOM DU MODELE
!
    if (char(1:1) .eq. ' ') then
        mod = modele
    else
        call dismoi('NOM_MODELE', char(1:8), 'CHARGE', repk=mod)
    endif
!
! --- LIGREL DU MODELE
!
    ligrmo = mod(1:8)//'.MODELE'
    call jeveuo(ligrmo//'.PRNM', 'L', jprnm)
!
! --- MAILLAGE DU MODELE
!
    call jeveuo(ligrmo//'.LGRF', 'L', jnoma)
    noma = zk8(jnoma)
    nomnoe = noma//'.NOMNOE'
!
! --- POUR CHAQUE NOEUD, ON VERIFIE SI LE DDL EST DESSUS
!
    do 20 i = 1, nbnd
        if (typend .eq. 'NUM') then
            ino = numnd(i)
        else if (typend.eq.'NOM') then
            call jenonu(jexnom(nomnoe, nomnd(i)), ino)
        else
            ASSERT(.false.)
        endif
        if (ino .ne. 0) then
            if (exisdg(zi(jprnm-1+ (ino-1)*nbec+1),icmp)) then
                resu(i) = 1
            else
                resu(i) = 0
            endif
        endif
 20 end do
!
    call jedema()
end subroutine
