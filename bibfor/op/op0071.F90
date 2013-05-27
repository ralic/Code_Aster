subroutine op0071()
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!
!     CALCUL PROJECTION MATRICE SUR BASE DE RITZ
!
!-----------------------------------------------------------------------
!
    include 'jeveux.h'
    include 'asterc/getres.h'
    include 'asterc/gettco.h'
    include 'asterc/getvid.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/projmc.h'
    include 'asterfort/projmr.h'
    include 'asterfort/rsorac.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    integer :: ibid, n1, n2, n3, n4, ier, nbmode, iadrif, neq, jsmde
    real(kind=8) :: rbid
    complex(kind=8) :: cbid
    character(len=1) :: typmat
    character(len=8) :: k8b, nomres, basemo, matras, numgen
    character(len=14) :: nu, numdd1, numdd2
    character(len=16) :: typres, nomcom, typbas
    character(len=14) :: nugene
    character(len=24) :: matric
    integer :: iarg
!-----------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
!
    call getres(nomres, typres, nomcom)
!
! --- RECUPERATION DES ARGUMENTS DE LA COMMANDE
!
    call getvid(' ', 'MATR_ASSE', 1, iarg, 1,&
                matras, n1)
    call getvid(' ', 'MATR_ASSE_GENE', 1, iarg, 1,&
                matras, n3)
    call getvid(' ', 'BASE', 1, iarg, 1,&
                basemo, n4)
    call getvid(' ', 'NUME_DDL_GENE', 1, iarg, 1,&
                numgen, n2)
    nugene=numgen
!
    call gettco(basemo, typbas)
    typmat= typres(16:16)
!
    if (n2 .ne. 0) then
    endif
!
!
!==================================================
!
    call rsorac(basemo, 'LONUTI', ibid, rbid, k8b,&
                cbid, rbid, 'ABSOLU', nbmode, 1,&
                ibid)
!
! RECUPERATION DU NOMBRE DE MODES REDUIT,
! NB_VECT DONNE PAR NUME_DDL_GENE
    call jeveuo(nugene//'.SMOS.SMDE', 'L', jsmde)
    nbmode = zi(jsmde-1+1)
!
!
    call dismoi('F', 'NOM_NUME_DDL', matras, 'MATR_ASSE', ibid,&
                numdd1, ier)
    call jeveuo(basemo//'           .REFD', 'L', iadrif)
    matric = zk24(iadrif)
    if (matric .ne. ' ') then
        call dismoi('F', 'NOM_NUME_DDL', matric, 'MATR_ASSE', ibid,&
                    numdd2, ier)
    else
        numdd2 = zk24(iadrif+1)(1:14)
    endif
    if (numdd1 .ne. numdd2) then
        call u2mess('I', 'ALGORITH9_39')
    endif
    nu = numdd1(1:14)
    call dismoi('F', 'NB_EQUA', matras, 'MATR_ASSE', neq,&
                k8b, ier)
!
    if (typmat .eq. 'R') then
        call projmr(matras, nomres, basemo, nugene, nu,&
                    neq, nbmode)
    else if (typmat.eq.'C') then
        call projmc(matras, nomres, basemo, nugene, nu,&
                    neq, nbmode)
    else
        call u2mesk('F', 'ALGORITH9_40', 1, typmat)
    endif
!
!
    call jedema()
end subroutine
