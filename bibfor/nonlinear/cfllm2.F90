subroutine cfllm2(resoco, resigr, neq, nesmax, nbliai,&
                  nbliac, llf, llf1, llf2, xmul)
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
!
#include "asterc/r8prem.h"
#include "asterfort/caladu.h"
#include "asterfort/calapr.h"
#include "asterfort/cfelpv.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/jedema.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/r8inir.h"
    character(len=24) :: resoco
    integer :: neq, nesmax
    integer :: nbliac, nbliai, llf, llf1, llf2
    real(kind=8) :: xmul
    real(kind=8) :: resigr
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
!
! CONSTRUCTION DE LA MATRICE TANGENTE DE FROTTEMENT - TERME NEGATIF
!
! ----------------------------------------------------------------------
!
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  RESIGR : RESI_GLOB_RELA
! IN  NEQ     : DIMENSION DU PROBLEME
! IN  NESMAX : NOMBRE MAX DE NOEUDS ESCLAVES
! IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
! IN  NBLIAI : NOMBRE DE LIAISONS
!
!
!
!
    integer :: ndlmax
    parameter   (ndlmax = 30)
    integer :: iliai1, iliai2, iliac
    character(len=19) :: liac, typl, mu, afmu
    integer :: jliac, jtypl, jmu, jafmu
    character(len=24) :: tacfin, appoin, apcofr, apddl
    integer :: jtacf, japptr, japcof, japddl
    integer :: ztacf
    character(len=19) :: fro2
    integer :: jfro2
    logical :: liaact
    character(len=2) :: typec0, typef0, typef1, typef2
    real(kind=8) :: lambdc, lambdf
    real(kind=8) :: coefff, coefte
    real(kind=8) :: alpha, beta
    character(len=2) :: typli2
    logical :: lelpiv, lelpi1, lelpi2
    real(kind=8) :: ajeufx, ajeufy, glis
    character(len=19) :: deplc
    integer :: jdepc
    integer :: nbddl, jdecal, btotal
    integer :: compt0
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    compt0 = 0
    btotal = nbliac + llf + llf1 + llf2
    typec0 = 'C0'
    typef0 = 'F0'
    typef1 = 'F1'
    typef2 = 'F2'
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    appoin = resoco(1:14)//'.APPOIN'
    apcofr = resoco(1:14)//'.APCOFR'
    apddl = resoco(1:14)//'.APDDL'
    liac = resoco(1:14)//'.LIAC'
    typl = resoco(1:14)//'.TYPL'
    tacfin = resoco(1:14)//'.TACFIN'
    mu = resoco(1:14)//'.MU'
    afmu = resoco(1:14)//'.AFMU'
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(apcofr, 'L', japcof)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(liac, 'L', jliac)
    call jeveuo(typl, 'L', jtypl)
    call jeveuo(tacfin, 'L', jtacf)
    call jeveuo(mu, 'L', jmu)
    call jeveuo(afmu, 'L', jafmu)
    ztacf = cfmmvd('ZTACF')
!
    fro2 = resoco(1:14)//'.FRO2'
!
! --- ACCES AUX CHAMPS DE TRAVAIL
! --- DEPLC : INCREMENT DE DEPLACEMENT CUMULE DEPUIS DEBUT
! ---         DU PAS DE TEMPS AVEC CORRECTION DU CONTACT
!
    deplc = resoco(1:14)//'.DEPC'
    call jeveuo(deplc (1:19)//'.VALE', 'L', jdepc)
!
! --- CALCUL DE LA MATRICE
!
    do 100 iliai1 = 1, nbliai
!
! ----- INFORMATIONS SUR LA LIAISON
!
        jdecal = zi(japptr+iliai1-1)
        nbddl = zi(japptr+iliai1)-zi(japptr+iliai1-1)
!
! ----- PARAMETRES
!
        coefff = zr(jtacf+ztacf*(iliai1-1)+1-1)
        coefte = zr(jtacf+ztacf*(iliai1-1)+4-1)
!
! ----- LIAISON PROVOQUANT UN PIVOT NUL ?
!
        call cfelpv(iliai1, typef0, resoco, nbliai, lelpiv)
        call cfelpv(iliai1, typef1, resoco, nbliai, lelpi1)
        call cfelpv(iliai1, typef2, resoco, nbliai, lelpi2)
!
! ----- ACCES COLONNE
!
        call jeveuo(jexnum(fro2, iliai1), 'E', jfro2)
        call r8inir(ndlmax, 0.d0, zr(jfro2), 1)
!
! ----- LA LIAISON EST-ELLE ACTIVE ?
!
        liaact = .false.
        compt0 = 0
        do 200 iliac = 1, btotal
            iliai2 = zi(jliac+iliac-1)
            typli2 = zk8(jtypl+iliac-1)(1:2)
            if (typli2 .eq. typec0) compt0 = compt0 + 1
            if (iliai2 .eq. iliai1) then
                liaact = .not.liaact
                lambdc = zr(jmu+compt0-1)
                lambdf = coefff*lambdc
            endif
200      continue
!
! ----- CALCUL DE LA COLONNE DE LA LIAISON ACTIVE
!
        if (.not.liaact) then
            call r8inir(ndlmax, 0.d0, zr(jfro2), 1)
        else
            if (lelpiv) then
                call r8inir(ndlmax, 0.d0, zr(jfro2), 1)
            else
!
! --------- CALCUL DES JEUX
!
                ajeufx = 0.d0
                ajeufy = 0.d0
                if (.not.lelpi1) then
                    call caladu(neq, nbddl, zr(japcof+jdecal), zi( japddl+jdecal), zr(jdepc),&
                                ajeufx)
                endif
                if (.not.lelpi2) then
                    call caladu(neq, nbddl, zr(japcof+jdecal+ndlmax* nesmax), zi(japddl+jdecal),&
                                zr(jdepc), ajeufy)
                endif
                glis = sqrt( ajeufx**2 + ajeufy**2 )
!
! --------- DETERMINATION DE BETA
!
                if (lambdf .eq. 0.d0) then
                    beta = 0.d0
                else
                    if (glis .le. r8prem()) then
                        beta = 0.d0
                    else
                        alpha = lambdf/glis
                        beta = sqrt(1.d0/(lambdf*glis))
                        if (alpha .gt. (xmul**2)) beta = 0.d0
                    endif
                endif
!
! --------- ON MULTIPIE BETA PAR COEF_MATR_FROT QUI VAUT 1
! --------- SI LE RESIDU EST PLUS PETIT QUE 1E-3
!
                if (resigr .ge. 1.d-03) then
                    beta = sqrt(coefte) * beta
                endif
!
! --------- CALCUL DE LA COLONNE
!
                call calapr(nbddl, beta, zr(jafmu), zi(japddl+jdecal), zr(jfro2))
            endif
!
        endif
!
! ----- LIBERATION COLONNE
!
        call jelibe(jexnum(fro2, iliai1))
100  end do
!
    call jedema()
!
end subroutine
