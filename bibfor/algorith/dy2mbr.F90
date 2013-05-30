subroutine dy2mbr(numddl, neq, lischa, freq, vediri,&
                  veneum, vevoch, vassec, j2nd)
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
    implicit      none
    include 'jeveux.h'
    include 'asterfort/ascomb.h'
    include 'asterfort/cnvesl.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/vtcreb.h'
    include 'asterfort/zinit.h'
    character(len=19) :: lischa
    integer :: neq, j2nd
    real(kind=8) :: freq
    character(len=14) :: numddl
    character(len=19) :: vediri, veneum, vevoch, vassec
!
! ----------------------------------------------------------------------
!
! DYNA_LINE_HARM
!
! CALCUL DU SECOND MEMBRE
!
! ----------------------------------------------------------------------
!
!
! IN  VEDIRI : VECT_ELEM DE L'ASSEMBLAGE DES ELEMENTS DE LAGRANGE
! IN  VENEUM : VECT_ELEM DE L'ASSEMBLAGE DES CHARGEMENTS DE NEUMANN
! IN  VEVOCH : VECT_ELEM DE L'ASSEMBLAGE DES CHARGEMENTS EVOL_CHAR
! IN  VASSEC : VECT_ELEM DE L'ASSEMBLAGE DES CHARGEMENTS VECT_ASSE_CHAR
! IN  LISCHA : SD LISTE DES CHARGES
! IN  NUMDDL : NOM DU NUME_DDL
! IN  FREQ   : VALEUR DE LA FREQUENCE
! IN  NEQ    : NOMBRE D'EQUATIONS DU SYSTEME
! IN  J2ND   : ADRESSE DU VECTEUR ASSEMBLE SECOND MEMBRE
!
!
!
!
    integer :: ieq
    integer :: j2nd1, j2nd2, j2nd3, j2nd4, j2nd5
    character(len=1) :: typres
    character(len=8) :: para
    character(len=19) :: cndiri, cnneum, cnvoch, cnveac, cnvass
    complex(kind=8) :: czero
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    typres = 'C'
    para = 'FREQ'
    czero = dcmplx(0.d0,0.d0)
    cndiri = '&&DY2MBR.CNDIRI'
    cnneum = '&&DY2MBR.CNNEUM'
    cnvoch = '&&DY2MBR.CNVOCH'
    cnveac = '&&DY2MBR.CNVEAC'
    cnvass = '&&DY2MBR.CNVASS'
    call vtcreb(cndiri, numddl, 'V', typres, neq)
    call vtcreb(cnneum, numddl, 'V', typres, neq)
    call vtcreb(cnvoch, numddl, 'V', typres, neq)
    call vtcreb(cnveac, numddl, 'V', typres, neq)
    call vtcreb(cnvass, numddl, 'V', typres, neq)
!
! --- VECTEUR RESULTANT
!
    call zinit(neq, czero, zc(j2nd), 1)
!
! --- ASSEMBLAGE DES CHARGEMENTS DE DIRICHLET
!
    call ascomb(lischa, vediri, typres, para, freq,&
                cndiri)
!
! --- ASSEMBLAGE DES CHARGEMENTS DE NEUMANN STANDARDS
!
    call ascomb(lischa, veneum, typres, para, freq,&
                cnneum)
!
! --- ASSEMBLAGE DU CHARGEMENT DE TYPE EVOL_CHAR
!
    call ascomb(lischa, vevoch, typres, para, freq,&
                cnvoch)
!
! --- ASSEMBLAGE DU CHARGEMENT DE TYPE VECT_ASSE_CHAR
!
    call ascomb(lischa, vassec, typres, para, freq,&
                cnveac)
!
! --- CHARGEMENT DE TYPE VECT_ASSE
!
    call cnvesl(lischa, typres, neq, para, freq,&
                cnvass)
!
! --- CUMUL DES DIFFERENTS TERMES DU SECOND MEMBRE DEFINITIF
!
    call jeveuo(cndiri(1:19)//'.VALE', 'L', j2nd1)
    call jeveuo(cnneum(1:19)//'.VALE', 'L', j2nd2)
    call jeveuo(cnvoch(1:19)//'.VALE', 'L', j2nd3)
    call jeveuo(cnveac(1:19)//'.VALE', 'L', j2nd4)
    call jeveuo(cnvass(1:19)//'.VALE', 'L', j2nd5)
    do 300 ieq = 1, neq
        zc(j2nd+ieq-1) = zc(j2nd1+ieq-1) + zc(j2nd2+ieq-1) + zc(j2nd3+ ieq-1) + zc(j2nd4+ieq-1) +&
                         & zc(j2nd5+ieq-1)
300  end do
!
    call detrsd('CHAMP_GD', cndiri)
    call detrsd('CHAMP_GD', cnneum)
    call detrsd('CHAMP_GD', cnvoch)
    call detrsd('CHAMP_GD', cnveac)
    call detrsd('CHAMP_GD', cnvass)
!
    call jedema()
!
end subroutine
