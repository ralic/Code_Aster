subroutine fglema(nbf, nbpoin, sig, defpla, temp,&
                  nommat, dom)
    implicit none
#include "jeveux.h"
#include "asterfort/fmdevi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/rccome.h"
#include "asterfort/rcvale.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=*) :: nommat
    real(kind=8) :: sig(*), defpla(*), temp(*), dom(*)
    integer :: nbf, nbpoin
!     ------------------------------------------------------------------
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
!     -----------------------------------------------------------------
!     CALCUL DU DOMMAGE DE LEMAITRE-SERMAGE
!     NOTE: Routine identique à "CALCUL DU DOMMAGE DE LEMAITRE"
!           avec prise en compte de l'exposant EXP_S dans la loi
!           d'évolution et VSEUIL dependant de la température.
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
! IN  NBF    : I   : NOMBRE DE FONCTIONS DECRIVANT LE CHARGEMENT
! IN  NBPOIN : I   : NOMBRE DE POINTS DE L'HISTOIRE DE CHARGEMENT
! IN  SIG    : R   : VALEURS TENSEUR CONTRAINTES
! IN  DEFPLA : R   : VALEURS DEFORMATION PLASTIQUE CUMULEE
! IN  TEMP   : R   : VALEURS TEMPERATURE
! IN  NOMMAT : K   : NOM DU MATERIAU
! OUT DOM    : R   : VALEURS DU DOMMAGE A CHAQUE INSTANT
!     ------------------------------------------------------------------
!
!
    integer :: icodre(3)
    character(len=8) :: nomres(3), nompar
    character(len=16) :: pheno, phenom
    real(kind=8) :: valmoi(3), valplu(3), temmoi, templu, pmoi, pplu
    real(kind=8) :: sihmoi, sihplu, seqmoi, seqplu, vmoi, vplu, vale
    real(kind=8) :: vseuil(1), exps(1), expo
    real(kind=8) :: null, un, deux, trois
!-----------------------------------------------------------------------
    integer :: i, ide, idev, nbpar
    real(kind=8) :: rbid, zero
!-----------------------------------------------------------------------
    data zero /1.d-13/
!
    call jemarq()
!
    rbid = 0.d0
    null = 0.d0
    un = 1.d0
    deux = 2.d0
    trois = 3.d0
!
    pheno = 'DOMMA_LEMAITRE'
    call rccome(nommat, pheno, phenom, icodre(1))
    if (icodre(1) .eq. 1) call u2mess('F', 'FATIGUE1_6')
    pheno = 'ELAS'
    call rccome(nommat, pheno, phenom, icodre(1))
    if (icodre(1) .eq. 1) call u2mess('F', 'FATIGUE1_7')
!
! --- CALCUL DU DOMMAGE ELEMENTAIRE
!
    call wkvect('&&FGLEMA.DEVIAT', 'V V R', nbf*nbpoin, idev)
    call fmdevi(nbf, nbpoin, sig, zr(idev))
!
    dom(1) = null
!
    do 10 i = 1, nbpoin-1
!
! --- RECUPERATION DE P,SIG,TEMP AUX INSTANTS TI ET TI+1
!
! --- RECUPERATION DE EXP_S
        nompar = '       '
        nomres(1) = 'EXP_S'
        call rcvale(nommat, 'DOMMA_LEMAITRE', 0, nompar, [rbid],&
                    1, nomres(1), exps(1), icodre(1), 2)
! --- RECUPERATION DU VSEUIL AUX INSTANTS TI+1
        nbpar = 1
        nompar = 'TEMP'
        temmoi = temp(i)
        templu = temp(i+1)
        nomres(1) = 'EPSP_SEU'
        call rcvale(nommat, 'DOMMA_LEMAITRE', nbpar, nompar, [templu],&
                    1, nomres(1), vseuil(1), icodre(1), 2)
!
        pmoi = defpla(i)
        pplu = defpla(i+1)
!
        if (pplu .gt. vseuil(1)-zero) then
!
! --- RECUPERATION DE E,NU,S,PD AUX INSTANTS TI ET TI+1
!
            nomres(1) = 'E'
            nomres(2) = 'NU'
            nomres(3) = 'S'
!
            call rcvale(nommat, 'ELAS', nbpar, nompar, [temmoi],&
                        2, nomres(1), valmoi(1), icodre(1), 2)
            call rcvale(nommat, 'ELAS', nbpar, nompar, [templu],&
                        2, nomres(1), valplu(1), icodre(1), 2)
            call rcvale(nommat, 'DOMMA_LEMAITRE', nbpar, nompar, [temmoi],&
                        1, nomres(3), valmoi(3), icodre(3), 2)
            call rcvale(nommat, 'DOMMA_LEMAITRE', nbpar, nompar, [templu],&
                        1, nomres(3), valplu(3), icodre(3), 2)
!
! --- CALCUL DE SIGMAH ET SIGMA EQUIVALENTE AUX INSTANTS TI ET TI+1
!
            ide = (i-1)*nbf
            sihmoi=(sig(ide+1)+sig(ide+2)+sig(ide+3))/trois
            sihmoi=sihmoi**2
            if (nbf .eq. 6) then
                seqmoi=(zr(idev+ide)*zr(idev+ide)+zr(idev+ide+1)*&
                zr(idev+ide+1)+zr(idev+ide+2)*zr(idev+ide+2))/deux&
                +zr(idev+ide+3)*zr(idev+ide+3)+zr(idev+ide+4)*&
                zr(idev+ide+4)+zr(idev+ide+5)*zr(idev+ide+5)
            else if (nbf.eq.4) then
                seqmoi=(zr(idev+ide)*zr(idev+ide)+zr(idev+ide+1)*&
                zr(idev+ide+1)+zr(idev+ide+2)*zr(idev+ide+2))/deux&
                +zr(idev+ide+3)*zr(idev+ide+3)
            endif
            seqmoi = trois*seqmoi
            ide = i*nbf
            sihplu=(sig(ide+1)+sig(ide+2)+sig(ide+3))/trois
            sihplu=sihplu**2
            if (nbf .eq. 6) then
                seqplu=(zr(idev+ide)*zr(idev+ide)+zr(idev+ide+1)*&
                zr(idev+ide+1)+zr(idev+ide+2)*zr(idev+ide+2))/deux&
                +zr(idev+ide+3)*zr(idev+ide+3)+zr(idev+ide+4)*&
                zr(idev+ide+4)+zr(idev+ide+5)*zr(idev+ide+5)
            else if (nbf.eq.4) then
                seqplu=(zr(idev+ide)*zr(idev+ide)+zr(idev+ide+1)*&
                zr(idev+ide+1)+zr(idev+ide+2)*zr(idev+ide+2))/deux&
                +zr(idev+ide+3)*zr(idev+ide+3)
            endif
            seqplu = trois*seqplu
!
            vmoi = ((un/trois)*(un+valmoi(2))*seqmoi)
            vmoi = vmoi+((trois/deux)*(un-deux*valmoi(2))*sihmoi)
            vmoi = (un/(valmoi(1)*valmoi(3)))*vmoi
            vmoi = vmoi**exps(1)
!
!
            vplu = ((un/trois)*(un+valplu(2))*seqplu)
            vplu = vplu+((trois/deux)*(un-deux*valplu(2))*sihplu)
            vplu = (un/(valplu(1)*valplu(3)))*vplu
            vplu = vplu**exps(1)
!
!
            vale = (un/deux)*(vmoi+vplu)
            vale = vale * (pplu-pmoi)
!
            expo = deux*exps(1)+un
            if (dom(i) .gt. un) then
                vale = -expo*vale
            else
                vale = (un-dom(i))**expo - expo*vale
            endif
!
            if (vale .gt. null) then
                dom(i+1) = un - (vale)**(un/expo)
            else
                dom(i+1) = un + (-vale)**(un/expo)
            endif
        else
            dom(i+1) = null
        endif
! La valeur de l'endommagement est bornée à 1
        if (dom(i+1) .gt. un) then
            dom(i+1) = un
        endif
10  end do
!
! --- MENAGE
    call jedetr('&&FGLEMA.DEVIAT')
    call jedema()
end subroutine
