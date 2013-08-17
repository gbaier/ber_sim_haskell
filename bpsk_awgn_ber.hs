import System.Random
import Control.Arrow
import qualified Data.Vector as V
import System.IO
-- error function
import Data.Number.Erf
-- deepseq deals with unwanted laziness
import Control.DeepSeq
-- provides foldl' and foldl1'
import Data.List

-- number of bits
nbits = 10^9

-- bit energy and noise power
ebn0 = [10**x | x <- [-1, -0.9..1.2]]

len = length ebn0

-- theoretical BER for bpsk
ber_theo = [ ((0.5*).erfc.sqrt) x | x <- ebn0 ]

-- variances
sigmas = [ 1/sqrt(2*x) | x <- ebn0]

-- converts a bit to a bpsk symbol
bit2bpsk :: Int -> Double
bit2bpsk bit = fromIntegral(2*bit-1)

-- converts a bpsk symbol to a bit
bpsk2bit :: Double -> Int
bpsk2bit = (round . (\i -> (i+1)/2) . signum)

-- additive white gaussian noise
awgn :: Double -> (Double, Double) -> Double
awgn sigma (noise, symbol) = sigma*noise+symbol

-- the complete chain with modulation, demodulation and AWGN with different variances
channel_and_mods = [ bpsk2bit.(awgn sigma).(\(!a,!b) -> (a, bit2bpsk b)) | sigma <- sigmas ]

-- channel, modulation and comparison
cmc :: [(Double, Int) -> Int]
cmc = [(c &&& snd >>> arr (\(y,z) -> if (y==z) then 0 else 1)) | c <- channel_and_mods ]

-- applies a list of function to one element
vecmap :: [a -> b] -> a -> [b]
vecmap [] _ = []
vecmap (f:fs) x = e `seq` e : vecmap fs x
    where e = f x

-- creates Gaussian distributed random numbers from 2 uniformly distributed
-- numbers using the Box-Muller-Algorithm 
boxmuller :: (Double, Double) -> (Double, Double)
boxmuller (u0, u1) = (z0, z1)
    where z0 = sqrt (-2*log u0)*cos (2*pi*u1)
          z1 = sqrt (-2*log u0)*sin (2*pi*u1)

-- strict vector adder
adder acc xs = (V.zipWith (+)) acc' xs
                 where acc' = acc `deepseq` acc
main = do
    gen_bits <- getStdGen
    gen_uniform1 <- newStdGen
    gen_uniform2 <- newStdGen
    let uniform1 = (randomRs (0,1) gen_uniform1 :: [Double])
    let uniform2 = (randomRs (0,1) gen_uniform2 :: [Double])
    let bits_tx = (randomRs (0,1) gen_bits :: [Int])
    let noise =  map (fst.boxmuller) $ zip uniform1 uniform2
    let bits_rx = map (vecmap cmc) $ zip noise bits_tx
    let tot_ber = V.toList $ foldl1' adder $ map V.fromList $ take nbits bits_rx
    let ber_sim = map (\n -> (fromIntegral n)/(fromIntegral nbits)) tot_ber
    let result = zip3 ebn0 ber_sim ber_theo
    mapM_ print result
    writeFile "bpsk_awgn_ber.txt" $ unlines $ map show result
