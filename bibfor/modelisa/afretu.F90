subroutine afretu(iprno, lonlis, klisno, noepou, noma,&
                  vale1, nbcoef, idec, coef, nomddl,&
                  typlag, lisrel)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/afrela.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/imprel.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: lonlis, iprno(*), idec, nbcoef
    real(kind=8) :: coef(nbcoef)
    character(len=2) :: typlag
    character(len=8) :: klisno(lonlis), noepou, noma, nomddl(nbcoef)
    character(len=24) :: vale1
    character(len=19) :: lisrel
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! -------------------------------------------------------
!     RACCORD (COQUE OU 3D)_TUYAU : UNE RELATION LINEAIRE
!
    integer :: ino, ival, idch1, nbterm, i, nbec, ier
    integer :: jlisno, jliscr, jliscc, jlisdi, jlisdm, jlisdl
    character(len=8) :: betaf, k8bid
    character(len=16) :: motfac
    character(len=24) :: noeuma
    real(kind=8) :: beta
    complex(kind=8) :: betac
!
    call jemarq()
!
    motfac = 'LIAISON_ELEM'
    betaf = '&FOZERO'
    beta = 0.0d0
    betac = (0.0d0,0.0d0)
    noeuma = noma//'.NOMNOE'
    motfac = 'LIAISON_ELEM'
    call jeveuo(vale1, 'L', idch1)
    call dismoi('F', 'NB_EC', 'DEPL_R', 'GRANDEUR', nbec,&
                k8bid, ier)
    if (nbec .gt. 10) then
        call u2mess('F', 'MODELISA_94')
    endif
!
! --- CREATION DES TABLEAUX DE TRAVAIL NECESSAIRES A L'AFFECTATION
! --- NBTERM : MAJORANT DU NOMBRE DE TERMES DANS UNE RELATION
!
    nbterm = 3*lonlis + nbcoef
! ---     VECTEUR DU NOM DES NOEUDS
    call wkvect('&&AFRETU.LISNO', 'V V K8', nbterm, jlisno)
! ---     VECTEUR DU NOM DES DDLS
    call wkvect('&&AFRETU.LISDDL', 'V V K8', nbterm, jlisdl)
! ---     VECTEUR DES COEFFICIENTS REELS
    call wkvect('&&AFRETU.COER', 'V V R', nbterm, jliscr)
! ---     VECTEUR DES COEFFICIENTS COMPLEXES
    call wkvect('&&AFRETU.COEC', 'V V C', nbterm, jliscc)
! ---     VECTEUR DES DIRECTIONS DES DDLS A CONTRAINDRE
    call wkvect('&&AFRETU.DIRECT', 'V V R', 3*nbterm, jlisdi)
! ---     VECTEUR DES DIMENSIONS DE CES DIRECTIONS
    call wkvect('&&AFRETU.DIME', 'V V I', nbterm, jlisdm)
!
!        RELATIONS ENTRE LES NOEUDS DE COQUE ET LE NOEUD NOEPOU
!        DE TUYAU SUR LE DDL NOMDDL
!
    do 10 i = 1, lonlis
        call jenonu(jexnom(noeuma, klisno(i)), ino)
!           ADRESSE DE LA PREMIERE CMP DU NOEUD INO DANS LES CHAMNO
        ival = iprno((ino-1)* (nbec+2)+1)
!
        zk8(jlisno+3* (i-1)+1-1) = klisno(i)
        zk8(jlisno+3* (i-1)+2-1) = klisno(i)
        zk8(jlisno+3* (i-1)+3-1) = klisno(i)
!
        zk8(jlisdl+3* (i-1)+1-1) = 'DX'
        zk8(jlisdl+3* (i-1)+2-1) = 'DY'
        zk8(jlisdl+3* (i-1)+3-1) = 'DZ'
!
! RACCORD  3D_TUYAU : IDEC=0 DANS TOUS LES APPELS A AFRETU
! RACCORD COQ_TUYAU : IDEC=0 OU 3 DANS LES APPELS A AFRETU
!
        zr(jliscr+3* (i-1)+1-1) = zr(idch1+ival-1+idec+0)
        zr(jliscr+3* (i-1)+2-1) = zr(idch1+ival-1+idec+1)
        zr(jliscr+3* (i-1)+3-1) = zr(idch1+ival-1+idec+2)
10  end do
!
    do 20 i = 1, nbcoef
        zk8(jlisno+3*lonlis+i-1) = noepou
        zk8(jlisdl+3*lonlis+i-1) = nomddl(i)
        zr(jliscr+3*lonlis+i-1) = coef(i)
20  end do
!
    call afrela(zr(jliscr), zc(jliscc), zk8(jlisdl), zk8(jlisno), zi(jlisdm),&
                zr(jlisdi), nbterm, beta, betac, betaf,&
                'REEL', 'REEL', typlag, 0.d0, lisrel)
    call imprel(motfac, nbterm, zr(jliscr), zk8(jlisdl), zk8(jlisno),&
                beta)
!
!
! --- MENAGE
!
    call jedetr('&&AFRETU.LISNO')
    call jedetr('&&AFRETU.LISDDL')
    call jedetr('&&AFRETU.COER')
    call jedetr('&&AFRETU.COEC')
    call jedetr('&&AFRETU.DIRECT')
    call jedetr('&&AFRETU.DIME')
!
    call jedema()
end subroutine
