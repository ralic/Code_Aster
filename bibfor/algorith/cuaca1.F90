subroutine cuaca1(deficu, resocu, solveu, lmat, cncine,&
                  nbliac, ajliai)
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
    implicit     none
    include 'jeveux.h'
!
    include 'asterfort/calatm.h'
    include 'asterfort/cudisi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/nmrldb.h'
    include 'asterfort/wkvect.h'
    character(len=24) :: deficu, resocu
    character(len=19) :: solveu, cncine
    integer :: lmat
    integer :: nbliac
    integer :: ajliai
!
! ----------------------------------------------------------------------
!
! ROUTINE LIAISON_UNILATER (RESOLUTION - A.C-1.AT)
!
! ROUTINE REALISANT LE CALCUL DE A.C-1.AT PAR RESOLUTION DE C.X=A(I)
!     A(I) -> I-EME COLONNE DE A
!     X    -> I-EME COLONNE DE C-1.A
!  LA ROUTINE EST OPTIMISEE PAR TRAITEMENT DES SECONDS MEMBRES PAR BLOCS
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICU : SD DE DEFINITION (ISSUE D'AFFE_CHAR_MECA)
! IN  RESOCU : SD DE TRAITEMENT NUMERIQUE
! IN  SOLVEU : SD SOLVEUR
! IN  LMAT   : DESCRIPTEUR DE LA MATR_ASSE DU SYSTEME MECANIQUE
! IN  CINE   : CHAM_NO CINEMATIQUE
! IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
! I/O AJLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON CORRECTE DU CALCUL
!              DE LA MATRICE DE CONTACT ACM1AT
!
!
!
!
    integer :: lg, il
    integer :: lliac, jdecal, nbddl
    integer :: neq, lgbloc, tampon
    integer :: nbsm, npas
    integer :: nrest, ipas, kk, iliac, npast
    character(len=19) :: liac, cm1a
    integer :: jliac, jcm1a
    character(len=24) :: apddl, apcoef, poinoe, chsecm
    integer :: japddl, japcoe, jpoi
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    cm1a = resocu(1:14)//'.CM1A'
    apddl = resocu(1:14)//'.APDDL'
    liac = resocu(1:14)//'.LIAC'
    apcoef = resocu(1:14)//'.APCOEF'
    poinoe = deficu(1:16)//'.POINOE'
!
! --- RECUPERATION D'OBJETS JEVEUX
!
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(liac, 'L', jliac)
    call jeveuo(apcoef, 'L', japcoe)
    call jeveuo(poinoe, 'L', jpoi)
!
! --- NOMBRE D'EQUATIONS DU SYSTEME
!
    neq = zi(lmat+2)
!
! ----------------------------------------------------------------------
! --- PAR METHODE DIRECTE AVEC BLOCS DE SECONDS MEMBRES
! ----------------------------------------------------------------------
!
! --- CALCUL DE LGBLOC
!
    lgbloc = cudisi(deficu,'NB_RESOL')
!
    nbsm = nbliac - ajliai
    npas = nbsm / lgbloc
    nrest = nbsm - lgbloc*npas
!
    if (nrest .gt. 0) then
        npast = npas + 1
    else
        npast = npas
    endif
    chsecm='&&CFACA1.TAMPON'
    call wkvect(chsecm, ' V V R ', neq*lgbloc, tampon)
!
!
    do 10 ipas = 1, npast
        lg = lgbloc
        if (npast .ne. npas .and. (ipas.eq.npast)) lg = nrest
!
        do 40 kk = 1, neq*lg
            zr(tampon-1+kk) = 0.0d0
40      continue
!
        do 20 il = 1, lg
            iliac = lgbloc* (ipas-1) + il + ajliai
            lliac = zi(jliac+iliac-1)
            jdecal = zi(jpoi+lliac-1)
            nbddl = zi(jpoi+lliac) - zi(jpoi+lliac-1)
!
! --- CALCUL DE LA COLONNE AT POUR LA LIAISON ACTIVE LLIAC EN CONTACT
!
            call calatm(neq, nbddl, 1.d0, zr(japcoe+jdecal), zi(japddl+ jdecal),&
                        zr(tampon+neq*(il-1)))
!
20      continue
!
! --- CALCUL DE C-1.AT (EN TENANT COMPTE DES CHARGES CINEMATIQUES)
!
        call nmrldb(solveu, lmat, zr(tampon), lg, cncine)
!
! --- RECOPIE
!
        do 50 il = 1, lg
            iliac = lgbloc* (ipas-1) + il + ajliai
            lliac = zi(jliac+iliac-1)
!
! --- AJOUT D'UNE LIAISON DE CONTACT
!
            call jeveuo(jexnum(cm1a, lliac), 'E', jcm1a)
            do 60 kk = 1, neq
                zr(jcm1a-1+kk) = zr(tampon-1+neq* (il-1)+kk)
60          continue
            call jelibe(jexnum(cm1a, lliac))
50      continue
10  end do
!
    ajliai = nbliac
!
! --- MENAGE
    call jedetr('&&CFACA1.TAMPON')
!
    call jedema()
!
end subroutine
