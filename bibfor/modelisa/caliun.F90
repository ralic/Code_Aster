subroutine caliun(charz, nomaz, nomoz)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
    include 'jeveux.h'
    include 'asterc/getfac.h'
    include 'asterfort/caraun.h'
    include 'asterfort/cfmmvd.h'
    include 'asterfort/creaun.h'
    include 'asterfort/elimun.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/listun.h'
    include 'asterfort/surfun.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: charz
    character(len=*) :: nomaz
    character(len=*) :: nomoz
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT
!
! TRAITEMENT DE LIAISON_UNILATERALE DANS DEFI_CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
!
!
!
!
    character(len=8) :: char, noma, nomo
    character(len=16) :: motfac
    integer :: iform
    integer :: nzocu, nnocu, ntcmp
    character(len=24) :: nolino, nopono
    character(len=24) :: lisnoe, poinoe
    character(len=24) :: nbgdcu, coefcu, compcu, multcu
    character(len=24) :: deficu, defico
    character(len=24) :: paraci, paracr, ndimcu
    integer :: jparci, jparcr, jdim
    integer :: zpari, zparr
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nomo = nomoz(1:8)
    char = charz
    noma = nomaz
    iform = 4
    motfac = 'ZONE'
    defico = char(1:8)//'.CONTACT'
    deficu = char(1:8)//'.UNILATE'
!
! --- RECUPERATION DU NOMBRE D'OCCURENCES
!
    call getfac(motfac, nzocu)
!
    if (nzocu .eq. 0) then
        goto 999
    endif
!
! --- CREATION SD PARAMETRES GENERAUX (NE DEPENDANT PAS DE LA ZONE)
!
    paraci = defico(1:16)//'.PARACI'
    zpari = cfmmvd('ZPARI')
    call wkvect(paraci, 'G V I', zpari, jparci)
    paracr = defico(1:16)//'.PARACR'
    zparr = cfmmvd('ZPARR')
    call wkvect(paracr, 'G V R', zparr, jparcr)
    zi(jparci+4-1) = iform
!
    ndimcu = deficu(1:16)//'.NDIMCU'
    call wkvect(ndimcu, 'G V I', 2, jdim)
!
! --- RECUPERATION CARACTERISTIQUES ELEMENTAIRES
!
    nbgdcu = '&&CALIUN.NBGDCU'
    compcu = '&&CARAUN.COMPCU'
    multcu = '&&CARAUN.MULTCU'
    coefcu = '&&CARAUN.COEFCU'
    call caraun(char, motfac, nzocu, nbgdcu, coefcu,&
                compcu, multcu, ntcmp)
!
! --- EXTRACTION DES NOEUDS
!
    nopono = '&&CALIUN.PONOEU'
    nolino = '&&CALIUN.LINOEU'
    call listun(noma, motfac, nzocu, nopono, nnocu,&
                nolino)
!
! --- ELIMINATION DES NOEUDS
!      SUPPRESSION DES DOUBLONS ENTRE MAILLE, GROUP_MA, NOEUD,GROUP_NO
!      SUPPRESSION DES NOEUDS DEFINIS DANS SANS_NOEUD ET SANS_GROUP_NO
!
    lisnoe = '&&CALIUN.LISNOE'
    poinoe = '&&CALIUN.POINOE'
    call elimun(noma, nomo, motfac, nzocu, nbgdcu,&
                compcu, nopono, nolino, lisnoe, poinoe,&
                nnocu)
!
! --- ELIMINATION DES COMPOSANTES ET CREATION FINALE DES SD
!
    call creaun(char, noma, nomo, nzocu, nnocu,&
                lisnoe, poinoe, nbgdcu, coefcu, compcu,&
                multcu)
!
! --- AFFICHAGE DES INFORMATIONS
!
    call surfun(char, noma)
!
! --- MENAGE
!
    call jedetr(nolino)
    call jedetr(nopono)
    call jedetr(lisnoe)
    call jedetr(poinoe)
    call jedetr(nbgdcu)
    call jedetr(coefcu)
    call jedetr(compcu)
    call jedetr(multcu)
!
999  continue
!
    call jedema()
!
end subroutine
