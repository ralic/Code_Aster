subroutine vedimd(nomo, lischa, instan, vecele)
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
!
    implicit      none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/calcul.h'
    include 'asterfort/corich.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/exisd.h'
    include 'asterfort/gcnco2.h'
    include 'asterfort/inical.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/lisico.h'
    include 'asterfort/lislch.h'
    include 'asterfort/lislco.h'
    include 'asterfort/lisllc.h'
    include 'asterfort/lisltc.h'
    include 'asterfort/lisnbg.h'
    include 'asterfort/lisnnb.h'
    include 'asterfort/lisnol.h'
    include 'asterfort/lisopt.h'
    include 'asterfort/mecact.h'
    include 'asterfort/mecoor.h'
    include 'asterfort/memare.h'
    include 'asterfort/reajre.h'
    character(len=19) :: lischa, vecele
    character(len=8) :: nomo
    real(kind=8) :: instan
!
! ----------------------------------------------------------------------
!
! CALCUL DES VECTEURS ELEMENTAIRES
!
! MECANIQUE - DIRICHLET
!
! ----------------------------------------------------------------------
!
!
! IN  NOMO   : NOM DU MODELE
! IN  LISCHA : SD LISTE DES CHARGES
! IN  INSTAN : INSTANT DE CALCUL
! OUT VECELE : VECT_ELEM RESULTAT
!
! ----------------------------------------------------------------------
!
    integer :: nbout, nbin
    parameter    (nbout=1, nbin=3)
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    character(len=8) :: nomch0
    character(len=8) :: k8bid, newnom
    character(len=16) :: option
    character(len=19) :: ligrmo, ligcal
    character(len=13) :: prefob
    character(len=19) :: chgeom, chtime
    character(len=19) :: carte
    character(len=8) :: parain, paraou, typech
    integer :: ibid, iret
    integer :: ichar, nbchar, codcha
    logical :: ldual
    character(len=24) :: nomlis
    integer :: jlisci, nbch, indxch
    complex(kind=8) :: c16bid
    integer :: nbdual
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    newnom = '.0000000'
    nomlis = '&&NOMLIS'
    call detrsd('VECT_ELEM', vecele)
!
! --- RECUPERATION DU MODELE
!
    ligrmo = nomo(1:8)//'.MODELE'
!
! --- INITIALISATION DES CHAMPS POUR CALCUL
!
    call inical(nbin, lpain, lchin, nbout, lpaout,&
                lchout)
!
! --- NOMBRE DE CHARGES
!
    call lisnnb(lischa, nbchar)
    if (nbchar .eq. 0) goto 99
!
! --- PRESENCE DE CE GENRE DE CHARGEMENT
!
    nbdual = lisnbg(lischa,'DIRI_DUAL')
    if (nbdual .eq. 0) goto 99
!
! --- ALLOCATION DU VECT_ELEM RESULTAT
!
    call memare('V', vecele, nomo, ' ', ' ',&
                'CHAR_MECA')
    call reajre(vecele, ' ', 'V')
!
! --- CHAMP DE GEOMETRIE
!
    call mecoor(nomo, chgeom)
!
! --- CARTE DE L'INSTANT
!
    chtime = '&&VEDIMD.CH_INST_R'
    call mecact('V', chtime, 'MODELE', ligrmo, 'INST_R',&
                1, 'INST', ibid, instan, c16bid,&
                k8bid)
!
! --- CHAMPS D'ENTREES STANDARDS
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PTEMPSR'
    lchin(2) = chtime
!
! --- LISTE DES INDEX DES CHARGES
!
    call lisnol(lischa, 'DIRI_DUAL', nomlis, nbch)
    call assert(nbch.eq.1)
    call jeveuo(nomlis, 'L', jlisci)
    indxch = zi(jlisci-1+1)
!
! --- CALCUL
!
    do 30 ichar = 1, nbchar
        call lislco(lischa, ichar, codcha)
        ldual = lisico('DIRI_DUAL',codcha)
        if (ldual) then
!
! ------- PREFIXE DE L'OBJET DE LA CHARGE
!
            call lisllc(lischa, ichar, prefob)
!
! ------- TYPE DE LA CHARGE
!
            call lisltc(lischa, ichar, typech)
!
! ------- NOM DE LA CHARGE
!
            call lislch(lischa, ichar, nomch0)
!
! ------- CALCUL SI CHARGE EXISTANTE
!
            call lisopt(prefob, nomo, typech, indxch, option,&
                        parain, paraou, carte, ligcal)
!
! ------- CARTE D'ENTREE
!
            lpain(3) = parain
            lchin(3) = carte
!
! ------- CARTE DE SORTIE
!
            lpaout(1) = paraou
            call gcnco2(newnom)
            lchout(1) = '&&VEDIMD.'//newnom(2:8)
            call corich('E', lchout(1), ichar, ibid)
!
! ------- CALCUL
!
            call calcul('S', option, ligcal, nbin, lchin,&
                        lpain, nbout, lchout, lpaout, 'V',&
                        'OUI')
!
! ------- RESU_ELEM DANS LE VECT_ELEM
!
            call exisd('CHAMP_GD', lchout(1), iret)
            call assert(iret.gt.0)
            call reajre(vecele, lchout(1), 'V')
        endif
30  end do
!
99  continue
!
    call jedetr(nomlis)
!
    call jedema()
end subroutine
