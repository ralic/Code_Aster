subroutine resgra(mat, matf, vcine, niter, epsi,&
                  criter, nsecm, rsolu, solveu, istop,&
                  iret)
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
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/csmbgg.h'
    include 'asterfort/gcpc.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/mrconl.h'
    include 'asterfort/mtdscr.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: mat, matf, vcine
    integer :: niter, nsecm
    real(kind=8) :: epsi, rsolu(*)
    character(len=19) :: criter, solveu
    integer :: istop, iret
!----------------------------------------------------------------------
!     ROUTINE DE HAUT NIVEAU DE RESOLUTION PAR UNE METHODE DE GRADIENT
!     CONJUGUE (GCPC)
!----------------------------------------------------------------------
! IN/JXIN  K19 MAT    : MATR_ASSE PREMIER MEMBRE DU SYSTEME LINEAIRE
! IN/JXIN  K19 MATF   : MATR_ASSE DE PRECONDITIONNEMENT
! IN/JXIN  K*  VCINE  : CHAMP ASSOCIE AUX CHARGES CINEMATIQUES (OU ' ')
! IN       I   NITER  : NOMBRE MAXIMUM D'ITERATIONS
! IN       R   EPSI   : PARAMETRE D'ERREUR
! IN/JXOUT K19 CRITER : SD_CRITER (CRITERES DE CONVERGENCE)
! IN       I   NSECM  : NOMBRE DE SECONDS MEMBRES
! IN/OUT   R   RSOLU(*,NSECM)  :
!        EN ENTREE : VECTEUR DE REELS CONTENANT LES SECONDS MEMBRES
!        EN SORTIE : VECTEUR DE REELS CONTENANT LES SOLUTIONS
! IN       K19 SOLVEU : SD_SOLVEUR
! IN       I   ISTOP  : COMPORTEMENT EN CAS D'ERREUR
! OUT      I   IRET   : CODE RETOUR
!----------------------------------------------------------------------
!----------------------------------------------------------------------
    complex(kind=8) :: cbid
!----------------------------------------------------------------------
!     VARIABLES LOCALES
!----------------------------------------------------------------------
    character(len=19) :: kstoc, kstocf
    character(len=19) :: vcin19, matas, matfac, smbr
    character(len=4) :: type
    character(len=24) :: precon
    integer :: ifm, niv, ier, idvalc, idin, idip, jsmde, neq, nblc, islvk
    integer :: ibid, idac, idinpc, idippc, idacpc, idw1, idw2, idw3
    integer :: jrefa, jrefaf, k, lmat, kdeb, ieq, idw4, ismbr
!
!----------------------------------------------------------------------
!     DEBUT
    call jemarq()
!
    call infniv(ifm, niv)
!
!     0- INITIALISATIONS :
!     -----------------------------------
    matas=mat
    matfac=matf
!
    call jeveuo(solveu//'.SLVK', 'L', islvk)
    precon=zk24(islvk-1+2)
!
!
!     1- MATRICE :
!     -----------------------------------
    call mtdscr(matas)
    call jeveuo(matas//'.&INT', 'L', lmat)
    neq=zi(lmat+2)
!
!
!     3- SI CHARGE CINEMATIQUE :
!     -----------------------------
    if (vcine .ne. ' ') then
        vcin19=vcine
        call jeexin(vcin19//'.VALE', ier)
        if (ier .eq. 0) call u2mesk('F', 'ALGELINE3_34', 1, vcin19)
        call jeveuo(vcin19//'.VALE', 'L', idvalc)
        do 10,k=1,nsecm
        kdeb=(k-1)*neq+1
        call csmbgg(lmat, rsolu(kdeb), zr(idvalc), cbid, cbid,&
                    'R')
10      continue
    endif
!
!
!     4- MISE A L'ECHELLE DES "LAGR" DANS LE SECOND MEMBRE :
!     ------------------------------------------------------
    call mrconl('MULT', lmat, 0, 'R', rsolu,&
                nsecm)
!
!
!     5- RECUPERATION DE LA MATRICE ASSEMBLEE :
!     ------------------------------------------------
    call jeveuo(matas//'.REFA', 'L', jrefa)
    kstoc=zk24(jrefa-1+2)(1:14)//'.SMOS'
    call jeexin(kstoc//'.SMDI', ier)
    if (ier .eq. 0) call u2mesk('F', 'ALGELINE3_21', 1, matas)
    call jeveuo(kstoc//'.SMDI', 'L', idin)
    call jeveuo(kstoc//'.SMHC', 'L', idip)
    call jeveuo(kstoc//'.SMDE', 'L', jsmde)
    neq=zi(jsmde-1+1)
    if (niter .eq. 0) niter=neq/2
    nblc=zi(jsmde-1+3)
    if (nblc .ne. 1) call u2mess('F', 'ALGELINE3_22')
    call jelira(jexnum(matas//'.VALM', 1), 'TYPE', ibid, type)
    if (type .ne. 'R') call u2mess('F', 'ALGELINE3_37')
!
    call jeveuo(jexnum(matas//'.VALM', 1), 'L', idac)
!
!
!     6- RECUPERATION DE LA MATRICE DE PRECONDITIONNEMENT:
!     -----------------------------------------------------
    if (precon(1:8) .eq. 'LDLT_INC') then
        call jeexin(matfac//'.REFA', ier)
        if (ier .eq. 0) call u2mess('F', 'ALGELINE3_38')
!
        call jeveuo(matfac//'.REFA', 'L', jrefaf)
        kstocf=zk24(jrefaf-1+2)(1:14)//'.SMOS'
        call jeveuo(kstocf//'.SMDI', 'L', idinpc)
        call jeveuo(kstocf//'.SMHC', 'L', idippc)
        call jeveuo(jexnum(matfac//'.VALM', 1), 'L', idacpc)
    else
        idinpc=1
        idippc=1
        idacpc=1
    endif
!
!
!     7- CREATION DE 3 VECTEURS DE TRAVAIL
!     ------------------------------------------------
    call wkvect('&&RESGRA.W1', 'V V R', neq, idw1)
    call wkvect('&&RESGRA.W2', 'V V R', neq, idw2)
    call wkvect('&&RESGRA.W3', 'V V R', neq, idw3)
!
!
!
!     9- RESOLUTION EFFECTIVE ---
!     ---------------------------------
    do 30,k=1,nsecm
    call wkvect('&&RESGRA.W4', 'V V R', neq, idw4)
!
!        ---- SOLUTION POUR MUMPS
    smbr='&&RESGRA.SMBR      '
    call wkvect(smbr//'.VALE', 'V V R', neq, ismbr)
!
    kdeb=(k-1)*neq+1
    call gcpc(neq, zi(idin), zi4(idip), zr(idac), zi(idinpc),&
              zi4(idippc), zr(idacpc), rsolu(kdeb), zr(idw4), zr(idw1),&
              zr(idw2), zr(idw3), 0, niter, epsi,&
              criter, solveu, matas, smbr, istop,&
              iret)
    do 20,ieq=1,neq
    rsolu(kdeb-1+ieq)=zr(idw4-1+ieq)
20  continue
    call jedetr('&&RESGRA.W4')
    call jedetr(smbr//'.VALE')
    30 end do
!
!
!     10- MISE A L'ECHELLE DES LAGRANGES DANS LA SOLUTION :
!     -----------------------------------------------------
    call mrconl('MULT', lmat, 0, 'R', rsolu,&
                nsecm)
!
!
    call jedetr('&&RESGRA.W1')
    call jedetr('&&RESGRA.W2')
    call jedetr('&&RESGRA.W3')
!
    call jedema()
end subroutine
